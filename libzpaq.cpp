/* libzpaq.cpp - LIBZPAQ Version 7.15 implementation - Aug. 17, 2016.

  libdivsufsort.c for divsufsort 2.00, included within, is
  (C) 2003-2008 Yuta Mori, all rights reserved.
  It is released under the MIT license as described in the comments
  at the beginning of that section.

  Some of the code for AES is from libtomcrypt 1.17 by Tom St. Denis
  and is public domain.

  The Salsa20/8 code for Scrypt is by D. Bernstein and is public domain.

  All of the remaining software is provided as-is, with no warranty.
  I, Matt Mahoney, release this software into
  the public domain. This applies worldwide.
  In some countries this may not be legally possible; if so:
  I grant anyone the right to use this software for any purpose,
  without any conditions, unless such conditions are required by law.

LIBZPAQ is a C++ library for compression and decompression of data
conforming to the ZPAQ level 2 standard. See http://mattmahoney.net/zpaq/
See libzpaq.h for additional documentation.
*/

#include "libzpaq.h"
#include <string.h>
#include <string>
#include <vector>
#include <stdio.h>

namespace libzpaq {

// Read 16 bit little-endian number
int toU16(const char* p) {
  return (p[0]&255)+256*(p[1]&255);
}

// Default read() and write()
int Reader::read(char* buf, int n) {
  int i=0, c;
  while (i<n && (c=get())>=0)
    buf[i++]=c;
  return i;
}

void Writer::write(const char* buf, int n) {
  for (int i=0; i<n; ++i)
    put(U8(buf[i]));
}

///////////////////////// allocx //////////////////////

// Allocate newsize > 0 bytes of executable memory and update
// p to point to it and newsize = n. Free any previously
// allocated memory first. If newsize is 0 then free only.
// Call error in case of failure. If NOJIT, ignore newsize
// and set p=0, n=0 without allocating memory.
void allocx(U8* &p, int &n, int newsize) {
  p=0;
  n=0;
}

//////////////////////////// SHA1 ////////////////////////////

// SHA1 code, see http://en.wikipedia.org/wiki/SHA-1

// Start a new hash
void SHA1::init() {
  len=0;
  h[0]=0x67452301;
  h[1]=0xEFCDAB89;
  h[2]=0x98BADCFE;
  h[3]=0x10325476;
  h[4]=0xC3D2E1F0;
  memset(w, 0, sizeof(w));
}

// Return old result and start a new hash
const char* SHA1::result() {

  // pad and append length
  const U64 s=len;
  put(0x80);
  while ((len&511)!=448)
    put(0);
  put(s>>56);
  put(s>>48);
  put(s>>40);
  put(s>>32);
  put(s>>24);
  put(s>>16);
  put(s>>8);
  put(s);

  // copy h to hbuf
  for (int i=0; i<5; ++i) {
    hbuf[4*i]=h[i]>>24;
    hbuf[4*i+1]=h[i]>>16;
    hbuf[4*i+2]=h[i]>>8;
    hbuf[4*i+3]=h[i];
  }

  // return hash prior to clearing state
  init();
  return hbuf;
}

// Hash buf[0..n-1]
void SHA1::write(const char* buf, int64_t n) {
  const unsigned char* p=(const unsigned char*) buf;
  for (; n>0 && (U32(len)&511)!=0; --n) put(*p++);
  for (; n>=64; n-=64) {
    for (int i=0; i<16; ++i)
      w[i]=p[0]<<24|p[1]<<16|p[2]<<8|p[3], p+=4;
    len+=512;
    process();
  }
  for (; n>0; --n) put(*p++);
}

// Hash 1 block of 64 bytes
void SHA1::process() {
  U32 a=h[0], b=h[1], c=h[2], d=h[3], e=h[4];
  static const U32 k[4]={0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC, 0xCA62C1D6};
  #define f(a,b,c,d,e,i) \
    if (i>=16) \
      w[(i)&15]^=w[(i-3)&15]^w[(i-8)&15]^w[(i-14)&15], \
      w[(i)&15]=w[(i)&15]<<1|w[(i)&15]>>31; \
    e+=(a<<5|a>>27)+k[(i)/20]+w[(i)&15] \
      +((i)%40>=20 ? b^c^d : i>=40 ? (b&c)|(d&(b|c)) : d^(b&(c^d))); \
    b=b<<30|b>>2;
  #define r(i) f(a,b,c,d,e,i) f(e,a,b,c,d,i+1) f(d,e,a,b,c,i+2) \
               f(c,d,e,a,b,i+3) f(b,c,d,e,a,i+4)
  r(0)  r(5)  r(10) r(15) r(20) r(25) r(30) r(35)
  r(40) r(45) r(50) r(55) r(60) r(65) r(70) r(75)
  #undef f
  #undef r
  h[0]+=a; h[1]+=b; h[2]+=c; h[3]+=d; h[4]+=e;
}

//////////////////////////// SHA256 //////////////////////////

void SHA256::init() {
  len0=len1=0;
  s[0]=0x6a09e667;
  s[1]=0xbb67ae85;
  s[2]=0x3c6ef372;
  s[3]=0xa54ff53a;
  s[4]=0x510e527f;
  s[5]=0x9b05688c;
  s[6]=0x1f83d9ab;
  s[7]=0x5be0cd19;
  memset(w, 0, sizeof(w));
}

void SHA256::process() {

  #define ror(a,b) ((a)>>(b)|(a<<(32-(b))))

  #define m(i) \
     w[(i)&15]+=w[(i-7)&15] \
       +(ror(w[(i-15)&15],7)^ror(w[(i-15)&15],18)^(w[(i-15)&15]>>3)) \
       +(ror(w[(i-2)&15],17)^ror(w[(i-2)&15],19)^(w[(i-2)&15]>>10))

  #define r(a,b,c,d,e,f,g,h,i) { \
    unsigned t1=ror(e,14)^e; \
    t1=ror(t1,5)^e; \
    h+=ror(t1,6)+((e&f)^(~e&g))+k[i]+w[(i)&15]; } \
    d+=h; \
    {unsigned t1=ror(a,9)^a; \
    t1=ror(t1,11)^a; \
    h+=ror(t1,2)+((a&b)^(c&(a^b))); }

  #define mr(a,b,c,d,e,f,g,h,i) m(i); r(a,b,c,d,e,f,g,h,i);

  #define r8(i) \
    r(a,b,c,d,e,f,g,h,i);   \
    r(h,a,b,c,d,e,f,g,i+1); \
    r(g,h,a,b,c,d,e,f,i+2); \
    r(f,g,h,a,b,c,d,e,i+3); \
    r(e,f,g,h,a,b,c,d,i+4); \
    r(d,e,f,g,h,a,b,c,i+5); \
    r(c,d,e,f,g,h,a,b,i+6); \
    r(b,c,d,e,f,g,h,a,i+7);

  #define mr8(i) \
    mr(a,b,c,d,e,f,g,h,i);   \
    mr(h,a,b,c,d,e,f,g,i+1); \
    mr(g,h,a,b,c,d,e,f,i+2); \
    mr(f,g,h,a,b,c,d,e,i+3); \
    mr(e,f,g,h,a,b,c,d,i+4); \
    mr(d,e,f,g,h,a,b,c,i+5); \
    mr(c,d,e,f,g,h,a,b,i+6); \
    mr(b,c,d,e,f,g,h,a,i+7);

  static const unsigned k[64]={
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2};

  unsigned a=s[0];
  unsigned b=s[1];
  unsigned c=s[2];
  unsigned d=s[3];
  unsigned e=s[4];
  unsigned f=s[5];
  unsigned g=s[6];
  unsigned h=s[7];

  r8(0);
  r8(8);
  mr8(16);
  mr8(24);
  mr8(32);
  mr8(40);
  mr8(48);
  mr8(56);

  s[0]+=a;
  s[1]+=b;
  s[2]+=c;
  s[3]+=d;
  s[4]+=e;
  s[5]+=f;
  s[6]+=g;
  s[7]+=h;

  #undef mr8
  #undef r8
  #undef mr
  #undef r
  #undef m
  #undef ror
};

// Return old result and start a new hash
const char* SHA256::result() {

  // pad and append length
  const unsigned s1=len1, s0=len0;
  put(0x80);
  while ((len0&511)!=448) put(0);
  put(s1>>24);
  put(s1>>16);
  put(s1>>8);
  put(s1);
  put(s0>>24);
  put(s0>>16);
  put(s0>>8);
  put(s0);

  // copy s to hbuf
  for (int i=0; i<8; ++i) {
    hbuf[4*i]=s[i]>>24;
    hbuf[4*i+1]=s[i]>>16;
    hbuf[4*i+2]=s[i]>>8;
    hbuf[4*i+3]=s[i];
  }

  // return hash prior to clearing state
  init();
  return hbuf;
}

//////////////////////////// AES /////////////////////////////

// Some AES code is derived from libtomcrypt 1.17 (public domain).

#define Te4_0 0x000000FF & Te4
#define Te4_1 0x0000FF00 & Te4
#define Te4_2 0x00FF0000 & Te4
#define Te4_3 0xFF000000 & Te4

// Extract byte n of x
static inline unsigned byte(unsigned x, unsigned n) {return (x>>(8*n))&255;}

// x = y[0..3] MSB first
static inline void LOAD32H(U32& x, const char* y) {
  const unsigned char* u=(const unsigned char*)y;
  x=u[0]<<24|u[1]<<16|u[2]<<8|u[3];
}

// y[0..3] = x MSB first
static inline void STORE32H(U32& x, unsigned char* y) {
  y[0]=x>>24;
  y[1]=x>>16;
  y[2]=x>>8;
  y[3]=x;
}

#define setup_mix(temp) \
  ((Te4_3[byte(temp, 2)]) ^ (Te4_2[byte(temp, 1)]) ^ \
   (Te4_1[byte(temp, 0)]) ^ (Te4_0[byte(temp, 3)]))

// Initialize encryption tables and round key. keylen is 16, 24, or 32.
AES_CTR::AES_CTR(const char* key, int keylen, const char* iv) {

  // Initialize IV (default 0)
  iv0=iv1=0;
  if (iv) {
    LOAD32H(iv0, iv);
    LOAD32H(iv1, iv+4);
  }

  // Initialize encryption tables
  for (int i=0; i<256; ++i) {
    unsigned s1=
    "\x63\x7c\x77\x7b\xf2\x6b\x6f\xc5\x30\x01\x67\x2b\xfe\xd7\xab\x76"
    "\xca\x82\xc9\x7d\xfa\x59\x47\xf0\xad\xd4\xa2\xaf\x9c\xa4\x72\xc0"
    "\xb7\xfd\x93\x26\x36\x3f\xf7\xcc\x34\xa5\xe5\xf1\x71\xd8\x31\x15"
    "\x04\xc7\x23\xc3\x18\x96\x05\x9a\x07\x12\x80\xe2\xeb\x27\xb2\x75"
    "\x09\x83\x2c\x1a\x1b\x6e\x5a\xa0\x52\x3b\xd6\xb3\x29\xe3\x2f\x84"
    "\x53\xd1\x00\xed\x20\xfc\xb1\x5b\x6a\xcb\xbe\x39\x4a\x4c\x58\xcf"
    "\xd0\xef\xaa\xfb\x43\x4d\x33\x85\x45\xf9\x02\x7f\x50\x3c\x9f\xa8"
    "\x51\xa3\x40\x8f\x92\x9d\x38\xf5\xbc\xb6\xda\x21\x10\xff\xf3\xd2"
    "\xcd\x0c\x13\xec\x5f\x97\x44\x17\xc4\xa7\x7e\x3d\x64\x5d\x19\x73"
    "\x60\x81\x4f\xdc\x22\x2a\x90\x88\x46\xee\xb8\x14\xde\x5e\x0b\xdb"
    "\xe0\x32\x3a\x0a\x49\x06\x24\x5c\xc2\xd3\xac\x62\x91\x95\xe4\x79"
    "\xe7\xc8\x37\x6d\x8d\xd5\x4e\xa9\x6c\x56\xf4\xea\x65\x7a\xae\x08"
    "\xba\x78\x25\x2e\x1c\xa6\xb4\xc6\xe8\xdd\x74\x1f\x4b\xbd\x8b\x8a"
    "\x70\x3e\xb5\x66\x48\x03\xf6\x0e\x61\x35\x57\xb9\x86\xc1\x1d\x9e"
    "\xe1\xf8\x98\x11\x69\xd9\x8e\x94\x9b\x1e\x87\xe9\xce\x55\x28\xdf"
    "\x8c\xa1\x89\x0d\xbf\xe6\x42\x68\x41\x99\x2d\x0f\xb0\x54\xbb\x16"
    [i]&255;
    unsigned s2=s1<<1;
    if (s2>=0x100) s2^=0x11b;
    unsigned s3=s1^s2;
    Te0[i]=s2<<24|s1<<16|s1<<8|s3;
    Te1[i]=s3<<24|s2<<16|s1<<8|s1;
    Te2[i]=s1<<24|s3<<16|s2<<8|s1;
    Te3[i]=s1<<24|s1<<16|s3<<8|s2;
    Te4[i]=s1<<24|s1<<16|s1<<8|s1;
  }

  // setup the forward key
  Nr = 10 + ((keylen/8)-2)*2;  // 10, 12, or 14 rounds
  int i = 0;
  U32* rk = &ek[0];
  U32 temp;
  static const U32 rcon[10] = {
    0x01000000UL, 0x02000000UL, 0x04000000UL, 0x08000000UL,
    0x10000000UL, 0x20000000UL, 0x40000000UL, 0x80000000UL,
    0x1B000000UL, 0x36000000UL};  // round constants

  LOAD32H(rk[0], key   );
  LOAD32H(rk[1], key +  4);
  LOAD32H(rk[2], key +  8);
  LOAD32H(rk[3], key + 12);
  if (keylen == 16) {
    for (;;) {
      temp  = rk[3];
      rk[4] = rk[0] ^ setup_mix(temp) ^ rcon[i];
      rk[5] = rk[1] ^ rk[4];
      rk[6] = rk[2] ^ rk[5];
      rk[7] = rk[3] ^ rk[6];
      if (++i == 10) {
         break;
      }
      rk += 4;
    }
  }
  else if (keylen == 24) {
    LOAD32H(rk[4], key + 16);
    LOAD32H(rk[5], key + 20);
    for (;;) {
      temp = rk[5];
      rk[ 6] = rk[ 0] ^ setup_mix(temp) ^ rcon[i];
      rk[ 7] = rk[ 1] ^ rk[ 6];
      rk[ 8] = rk[ 2] ^ rk[ 7];
      rk[ 9] = rk[ 3] ^ rk[ 8];
      if (++i == 8) {
        break;
      }
      rk[10] = rk[ 4] ^ rk[ 9];
      rk[11] = rk[ 5] ^ rk[10];
      rk += 6;
    }
  }
  else if (keylen == 32) {
    LOAD32H(rk[4], key + 16);
    LOAD32H(rk[5], key + 20);
    LOAD32H(rk[6], key + 24);
    LOAD32H(rk[7], key + 28);
    for (;;) {
      temp = rk[7];
      rk[ 8] = rk[ 0] ^ setup_mix(temp) ^ rcon[i];
      rk[ 9] = rk[ 1] ^ rk[ 8];
      rk[10] = rk[ 2] ^ rk[ 9];
      rk[11] = rk[ 3] ^ rk[10];
      if (++i == 7) {
        break;
      }
      temp = rk[11];
      rk[12] = rk[ 4] ^ setup_mix(temp<<24|temp>>8);
      rk[13] = rk[ 5] ^ rk[12];
      rk[14] = rk[ 6] ^ rk[13];
      rk[15] = rk[ 7] ^ rk[14];
      rk += 8;
    }
  }
}

// Encrypt to ct[16]
void AES_CTR::encrypt(U32 s0, U32 s1, U32 s2, U32 s3, unsigned char* ct) {
  int r = Nr >> 1;
  U32 *rk = &ek[0];
  U32 t0=0, t1=0, t2=0, t3=0;
  s0 ^= rk[0];
  s1 ^= rk[1];
  s2 ^= rk[2];
  s3 ^= rk[3];
  for (;;) {
    t0 =
      Te0[byte(s0, 3)] ^
      Te1[byte(s1, 2)] ^
      Te2[byte(s2, 1)] ^
      Te3[byte(s3, 0)] ^
      rk[4];
    t1 =
      Te0[byte(s1, 3)] ^
      Te1[byte(s2, 2)] ^
      Te2[byte(s3, 1)] ^
      Te3[byte(s0, 0)] ^
      rk[5];
    t2 =
      Te0[byte(s2, 3)] ^
      Te1[byte(s3, 2)] ^
      Te2[byte(s0, 1)] ^
      Te3[byte(s1, 0)] ^
      rk[6];
    t3 =
      Te0[byte(s3, 3)] ^
      Te1[byte(s0, 2)] ^
      Te2[byte(s1, 1)] ^
      Te3[byte(s2, 0)] ^
      rk[7];

    rk += 8;
    if (--r == 0) {
      break;
    }

    s0 =
      Te0[byte(t0, 3)] ^
      Te1[byte(t1, 2)] ^
      Te2[byte(t2, 1)] ^
      Te3[byte(t3, 0)] ^
      rk[0];
    s1 =
      Te0[byte(t1, 3)] ^
      Te1[byte(t2, 2)] ^
      Te2[byte(t3, 1)] ^
      Te3[byte(t0, 0)] ^
      rk[1];
    s2 =
      Te0[byte(t2, 3)] ^
      Te1[byte(t3, 2)] ^
      Te2[byte(t0, 1)] ^
      Te3[byte(t1, 0)] ^
      rk[2];
    s3 =
      Te0[byte(t3, 3)] ^
      Te1[byte(t0, 2)] ^
      Te2[byte(t1, 1)] ^
      Te3[byte(t2, 0)] ^
      rk[3];
  }

  // apply last round and map cipher state to byte array block:
  s0 =
    (Te4_3[byte(t0, 3)]) ^
    (Te4_2[byte(t1, 2)]) ^
    (Te4_1[byte(t2, 1)]) ^
    (Te4_0[byte(t3, 0)]) ^
    rk[0];
  STORE32H(s0, ct);
  s1 =
    (Te4_3[byte(t1, 3)]) ^
    (Te4_2[byte(t2, 2)]) ^
    (Te4_1[byte(t3, 1)]) ^
    (Te4_0[byte(t0, 0)]) ^
    rk[1];
  STORE32H(s1, ct+4);
  s2 =
    (Te4_3[byte(t2, 3)]) ^
    (Te4_2[byte(t3, 2)]) ^
    (Te4_1[byte(t0, 1)]) ^
    (Te4_0[byte(t1, 0)]) ^
    rk[2];
  STORE32H(s2, ct+8);
  s3 =
    (Te4_3[byte(t3, 3)]) ^
    (Te4_2[byte(t0, 2)]) ^
    (Te4_1[byte(t1, 1)]) ^
    (Te4_0[byte(t2, 0)]) ^ 
    rk[3];
  STORE32H(s3, ct+12);
}

// Encrypt or decrypt slice buf[0..n-1] at offset by XOR with AES(i) where
// i is the 128 bit big-endian distance from the start in 16 byte blocks.
void AES_CTR::encrypt(char* buf, int n, U64 offset) {
  for (U64 i=offset/16; i<=(offset+n)/16; ++i) {
    unsigned char ct[16];
    encrypt(iv0, iv1, i>>32, i, ct);
    for (int j=0; j<16; ++j) {
      const int k=i*16-offset+j;
      if (k>=0 && k<n)
        buf[k]^=ct[j];
    }
  }
}

#undef setup_mix
#undef Te4_3
#undef Te4_2
#undef Te4_1
#undef Te4_0

//////////////////////////// stretchKey //////////////////////

// PBKDF2(pw[0..pwlen], salt[0..saltlen], c) to buf[0..dkLen-1]
// using HMAC-SHA256, for the special case of c = 1 iterations
// output size dkLen a multiple of 32, and pwLen <= 64.
static void pbkdf2(const char* pw, int pwLen, const char* salt, int saltLen,
                   int c, char* buf, int dkLen) {

  libzpaq::SHA256 sha256;
  char b[32];
  for (int i=1; i*32<=dkLen; ++i) {
    for (int j=0; j<pwLen; ++j) sha256.put(pw[j]^0x36);
    for (int j=pwLen; j<64; ++j) sha256.put(0x36);
    for (int j=0; j<saltLen; ++j) sha256.put(salt[j]);
    for (int j=24; j>=0; j-=8) sha256.put(i>>j);
    memcpy(b, sha256.result(), 32);
    for (int j=0; j<pwLen; ++j) sha256.put(pw[j]^0x5c);
    for (int j=pwLen; j<64; ++j) sha256.put(0x5c);
    for (int j=0; j<32; ++j) sha256.put(b[j]);
    memcpy(buf+i*32-32, sha256.result(), 32);
  }
}

// Hash b[0..15] using 8 rounds of salsa20
// Modified from http://cr.yp.to/salsa20.html (public domain) to 8 rounds
static void salsa8(U32* b) {
  unsigned x[16]={0};
  memcpy(x, b, 64);
  for (int i=0; i<4; ++i) {
    #define R(a,b) (((a)<<(b))+((a)>>(32-b)))
    x[ 4] ^= R(x[ 0]+x[12], 7);  x[ 8] ^= R(x[ 4]+x[ 0], 9);
    x[12] ^= R(x[ 8]+x[ 4],13);  x[ 0] ^= R(x[12]+x[ 8],18);
    x[ 9] ^= R(x[ 5]+x[ 1], 7);  x[13] ^= R(x[ 9]+x[ 5], 9);
    x[ 1] ^= R(x[13]+x[ 9],13);  x[ 5] ^= R(x[ 1]+x[13],18);
    x[14] ^= R(x[10]+x[ 6], 7);  x[ 2] ^= R(x[14]+x[10], 9);
    x[ 6] ^= R(x[ 2]+x[14],13);  x[10] ^= R(x[ 6]+x[ 2],18);
    x[ 3] ^= R(x[15]+x[11], 7);  x[ 7] ^= R(x[ 3]+x[15], 9);
    x[11] ^= R(x[ 7]+x[ 3],13);  x[15] ^= R(x[11]+x[ 7],18);
    x[ 1] ^= R(x[ 0]+x[ 3], 7);  x[ 2] ^= R(x[ 1]+x[ 0], 9);
    x[ 3] ^= R(x[ 2]+x[ 1],13);  x[ 0] ^= R(x[ 3]+x[ 2],18);
    x[ 6] ^= R(x[ 5]+x[ 4], 7);  x[ 7] ^= R(x[ 6]+x[ 5], 9);
    x[ 4] ^= R(x[ 7]+x[ 6],13);  x[ 5] ^= R(x[ 4]+x[ 7],18);
    x[11] ^= R(x[10]+x[ 9], 7);  x[ 8] ^= R(x[11]+x[10], 9);
    x[ 9] ^= R(x[ 8]+x[11],13);  x[10] ^= R(x[ 9]+x[ 8],18);
    x[12] ^= R(x[15]+x[14], 7);  x[13] ^= R(x[12]+x[15], 9);
    x[14] ^= R(x[13]+x[12],13);  x[15] ^= R(x[14]+x[13],18);
    #undef R
  }
  for (int i=0; i<16; ++i) b[i]+=x[i];
}

// BlockMix_{Salsa20/8, r} on b[0..128*r-1]
static void blockmix(U32* b, int r) {
  U32 x[16];
  U32 y[256];
  memcpy(x, b+32*r-16, 64);
  for (int i=0; i<2*r; ++i) {
    for (int j=0; j<16; ++j) x[j]^=b[i*16+j];
    salsa8(x);
    memcpy(&y[i*16], x, 64);
  }
  for (int i=0; i<r; ++i) memcpy(b+i*16, &y[i*32], 64);
  for (int i=0; i<r; ++i) memcpy(b+(i+r)*16, &y[i*32+16], 64);
}

// Mix b[0..128*r-1]. Uses 128*r*n bytes of memory and O(r*n) time
static void smix(char* b, int r, int n) {
  libzpaq::Array<U32> x(32*r), v(32*r*n);
  for (int i=0; i<r*128; ++i) x[i/4]+=(b[i]&255)<<i%4*8;
  for (int i=0; i<n; ++i) {
    memcpy(&v[i*r*32], &x[0], r*128);
    blockmix(&x[0], r);
  }
  for (int i=0; i<n; ++i) {
    U32 j=x[(2*r-1)*16]&(n-1);
    for (int k=0; k<r*32; ++k) x[k]^=v[j*r*32+k];
    blockmix(&x[0], r);
  }
  for (int i=0; i<r*128; ++i) b[i]=x[i/4]>>(i%4*8);
}

// Strengthen password pw[0..pwlen-1] and salt[0..saltlen-1]
// to produce key buf[0..buflen-1]. Uses O(n*r*p) time and 128*r*n bytes
// of memory. n must be a power of 2 and r <= 8.
void scrypt(const char* pw, int pwlen,
            const char* salt, int saltlen,
            int n, int r, int p, char* buf, int buflen) {
  libzpaq::Array<char> b(p*r*128);
  pbkdf2(pw, pwlen, salt, saltlen, 1, &b[0], p*r*128);
  for (int i=0; i<p; ++i) smix(&b[i*r*128], r, n);
  pbkdf2(pw, pwlen, &b[0], p*r*128, 1, buf, buflen);
}

// Stretch key in[0..31], assumed to be SHA256(password), with
// NUL terminate salt to produce new key out[0..31]
void stretchKey(char* out, const char* in, const char* salt) {
  scrypt(in, 32, salt, 32, 1<<14, 8, 1, out, 32);
}

//////////////////////////// random //////////////////////////

// Put n cryptographic random bytes in buf[0..n-1].
// The first byte will not be 'z' or '7' (start of a ZPAQ archive).
// For a pure random number, discard the first byte.
// In VC++, must link to advapi32.lib.

void random(char* buf, int n) {
  FILE* in=fopen("/dev/urandom", "rb");
  if (in && int(fread(buf, 1, n, in))==n)
    fclose(in);
  else {
    error("key generation failed");
  }
  if (n>=1 && (buf[0]=='z' || buf[0]=='7'))
    buf[0]^=0x80;
}

//////////////////////////// Component ///////////////////////

// A Component is a context model, indirect context model, match model,
// fixed weight mixer, adaptive 2 input mixer without or with current
// partial byte as context, adaptive m input mixer (without or with),
// or SSE (without or with).

const int compsize[256]={0,2,3,2,3,4,6,6,3,5};

void Component::init() {
  limit=cxt=a=b=c=0;
  cm.resize(0);
  ht.resize(0);
  a16.resize(0);
}

////////////////////////// StateTable ////////////////////////

// sns[i*4] -> next state if 0, next state if 1, n0, n1
static const U8 sns[1024]={
     1,     2,     0,     0,     3,     5,     1,     0,
     4,     6,     0,     1,     7,     9,     2,     0,
     8,    11,     1,     1,     8,    11,     1,     1,
    10,    12,     0,     2,    13,    15,     3,     0,
    14,    17,     2,     1,    14,    17,     2,     1,
    16,    19,     1,     2,    16,    19,     1,     2,
    18,    20,     0,     3,    21,    23,     4,     0,
    22,    25,     3,     1,    22,    25,     3,     1,
    24,    27,     2,     2,    24,    27,     2,     2,
    26,    29,     1,     3,    26,    29,     1,     3,
    28,    30,     0,     4,    31,    33,     5,     0,
    32,    35,     4,     1,    32,    35,     4,     1,
    34,    37,     3,     2,    34,    37,     3,     2,
    36,    39,     2,     3,    36,    39,     2,     3,
    38,    41,     1,     4,    38,    41,     1,     4,
    40,    42,     0,     5,    43,    33,     6,     0,
    44,    47,     5,     1,    44,    47,     5,     1,
    46,    49,     4,     2,    46,    49,     4,     2,
    48,    51,     3,     3,    48,    51,     3,     3,
    50,    53,     2,     4,    50,    53,     2,     4,
    52,    55,     1,     5,    52,    55,     1,     5,
    40,    56,     0,     6,    57,    45,     7,     0,
    58,    47,     6,     1,    58,    47,     6,     1,
    60,    63,     5,     2,    60,    63,     5,     2,
    62,    65,     4,     3,    62,    65,     4,     3,
    64,    67,     3,     4,    64,    67,     3,     4,
    66,    69,     2,     5,    66,    69,     2,     5,
    52,    71,     1,     6,    52,    71,     1,     6,
    54,    72,     0,     7,    73,    59,     8,     0,
    74,    61,     7,     1,    74,    61,     7,     1,
    76,    63,     6,     2,    76,    63,     6,     2,
    78,    81,     5,     3,    78,    81,     5,     3,
    80,    83,     4,     4,    80,    83,     4,     4,
    82,    85,     3,     5,    82,    85,     3,     5,
    66,    87,     2,     6,    66,    87,     2,     6,
    68,    89,     1,     7,    68,    89,     1,     7,
    70,    90,     0,     8,    91,    59,     9,     0,
    92,    77,     8,     1,    92,    77,     8,     1,
    94,    79,     7,     2,    94,    79,     7,     2,
    96,    81,     6,     3,    96,    81,     6,     3,
    98,   101,     5,     4,    98,   101,     5,     4,
   100,   103,     4,     5,   100,   103,     4,     5,
    82,   105,     3,     6,    82,   105,     3,     6,
    84,   107,     2,     7,    84,   107,     2,     7,
    86,   109,     1,     8,    86,   109,     1,     8,
    70,   110,     0,     9,   111,    59,    10,     0,
   112,    77,     9,     1,   112,    77,     9,     1,
   114,    97,     8,     2,   114,    97,     8,     2,
   116,    99,     7,     3,   116,    99,     7,     3,
    62,   101,     6,     4,    62,   101,     6,     4,
    80,    83,     5,     5,    80,    83,     5,     5,
   100,    67,     4,     6,   100,    67,     4,     6,
   102,   119,     3,     7,   102,   119,     3,     7,
   104,   121,     2,     8,   104,   121,     2,     8,
    86,   123,     1,     9,    86,   123,     1,     9,
    70,   124,     0,    10,   125,    59,    11,     0,
   126,    77,    10,     1,   126,    77,    10,     1,
   128,    97,     9,     2,   128,    97,     9,     2,
    60,    63,     8,     3,    60,    63,     8,     3,
    66,    69,     3,     8,    66,    69,     3,     8,
   104,   131,     2,     9,   104,   131,     2,     9,
    86,   133,     1,    10,    86,   133,     1,    10,
    70,   134,     0,    11,   135,    59,    12,     0,
   136,    77,    11,     1,   136,    77,    11,     1,
   138,    97,    10,     2,   138,    97,    10,     2,
   104,   141,     2,    10,   104,   141,     2,    10,
    86,   143,     1,    11,    86,   143,     1,    11,
    70,   144,     0,    12,   145,    59,    13,     0,
   146,    77,    12,     1,   146,    77,    12,     1,
   148,    97,    11,     2,   148,    97,    11,     2,
   104,   151,     2,    11,   104,   151,     2,    11,
    86,   153,     1,    12,    86,   153,     1,    12,
    70,   154,     0,    13,   155,    59,    14,     0,
   156,    77,    13,     1,   156,    77,    13,     1,
   158,    97,    12,     2,   158,    97,    12,     2,
   104,   161,     2,    12,   104,   161,     2,    12,
    86,   163,     1,    13,    86,   163,     1,    13,
    70,   164,     0,    14,   165,    59,    15,     0,
   166,    77,    14,     1,   166,    77,    14,     1,
   168,    97,    13,     2,   168,    97,    13,     2,
   104,   171,     2,    13,   104,   171,     2,    13,
    86,   173,     1,    14,    86,   173,     1,    14,
    70,   174,     0,    15,   175,    59,    16,     0,
   176,    77,    15,     1,   176,    77,    15,     1,
   178,    97,    14,     2,   178,    97,    14,     2,
   104,   181,     2,    14,   104,   181,     2,    14,
    86,   183,     1,    15,    86,   183,     1,    15,
    70,   184,     0,    16,   185,    59,    17,     0,
   186,    77,    16,     1,   186,    77,    16,     1,
    74,    97,    15,     2,    74,    97,    15,     2,
   104,    89,     2,    15,   104,    89,     2,    15,
    86,   187,     1,    16,    86,   187,     1,    16,
    70,   188,     0,    17,   189,    59,    18,     0,
   190,    77,    17,     1,    86,   191,     1,    17,
    70,   192,     0,    18,   193,    59,    19,     0,
   194,    77,    18,     1,    86,   195,     1,    18,
    70,   196,     0,    19,   193,    59,    20,     0,
   197,    77,    19,     1,    86,   198,     1,    19,
    70,   196,     0,    20,   199,    77,    20,     1,
    86,   200,     1,    20,   201,    77,    21,     1,
    86,   202,     1,    21,   203,    77,    22,     1,
    86,   204,     1,    22,   205,    77,    23,     1,
    86,   206,     1,    23,   207,    77,    24,     1,
    86,   208,     1,    24,   209,    77,    25,     1,
    86,   210,     1,    25,   211,    77,    26,     1,
    86,   212,     1,    26,   213,    77,    27,     1,
    86,   214,     1,    27,   215,    77,    28,     1,
    86,   216,     1,    28,   217,    77,    29,     1,
    86,   218,     1,    29,   219,    77,    30,     1,
    86,   220,     1,    30,   221,    77,    31,     1,
    86,   222,     1,    31,   223,    77,    32,     1,
    86,   224,     1,    32,   225,    77,    33,     1,
    86,   226,     1,    33,   227,    77,    34,     1,
    86,   228,     1,    34,   229,    77,    35,     1,
    86,   230,     1,    35,   231,    77,    36,     1,
    86,   232,     1,    36,   233,    77,    37,     1,
    86,   234,     1,    37,   235,    77,    38,     1,
    86,   236,     1,    38,   237,    77,    39,     1,
    86,   238,     1,    39,   239,    77,    40,     1,
    86,   240,     1,    40,   241,    77,    41,     1,
    86,   242,     1,    41,   243,    77,    42,     1,
    86,   244,     1,    42,   245,    77,    43,     1,
    86,   246,     1,    43,   247,    77,    44,     1,
    86,   248,     1,    44,   249,    77,    45,     1,
    86,   250,     1,    45,   251,    77,    46,     1,
    86,   252,     1,    46,   253,    77,    47,     1,
    86,   254,     1,    47,   253,    77,    48,     1,
    86,   254,     1,    48,     0,     0,     0,     0
};

// Initialize next state table ns[state*4] -> next if 0, next if 1, n0, n1
StateTable::StateTable() {
  memcpy(ns, sns, sizeof(ns));
}

/////////////////////////// ZPAQL //////////////////////////

// Write header to out2, return true if HCOMP/PCOMP section is present.
// If pp is true, then write only the postprocessor code.
bool ZPAQL::write(Writer* out2, bool pp) {
  if (header.size()<=6) return false;
  if (!pp) {  // if not a postprocessor then write COMP
    for (int i=0; i<cend; ++i)
      out2->put(header[i]);
  }
  else {  // write PCOMP size only
    out2->put((hend-hbegin)&255);
    out2->put((hend-hbegin)>>8);
  }
  for (int i=hbegin; i<hend; ++i)
    out2->put(header[i]);
  return true;
}

// Read header from in2
int ZPAQL::read(Reader* in2) {

  // Get header size and allocate
  int hsize=in2->get();
  hsize+=in2->get()*256;
  header.resize(hsize+300);
  cend=hbegin=hend=0;
  header[cend++]=hsize&255;
  header[cend++]=hsize>>8;
  while (cend<7) header[cend++]=in2->get(); // hh hm ph pm n

  // Read COMP
  int n=header[cend-1];
  for (int i=0; i<n; ++i) {
    int type=in2->get();  // component type
    if (type<0 || type>255) error("unexpected end of file");
    header[cend++]=type;  // component type
    int size=compsize[type];
    if (size<1) error("Invalid component type");
    if (cend+size>hsize) error("COMP overflows header");
    for (int j=1; j<size; ++j)
      header[cend++]=in2->get();
  }
  if ((header[cend++]=in2->get())!=0) error("missing COMP END");

  // Insert a guard gap and read HCOMP
  hbegin=hend=cend+128;
  if (hend>hsize+129) error("missing HCOMP");
  while (hend<hsize+129) {
    int op=in2->get();
    if (op==-1) error("unexpected end of file");
    header[hend++]=op;
  }
  if ((header[hend++]=in2->get())!=0) error("missing HCOMP END");
  allocx(rcode, rcode_size, 0);  // clear JIT code
  return cend+hend-hbegin;
}

// Free memory, but preserve output, sha1 pointers
void ZPAQL::clear() {
  cend=hbegin=hend=0;  // COMP and HCOMP locations
  a=b=c=d=f=pc=0;      // machine state
  header.resize(0);
  h.resize(0);
  m.resize(0);
  r.resize(0);
  allocx(rcode, rcode_size, 0);
}

// Constructor
ZPAQL::ZPAQL() {
  output=0;
  sha1=0;
  rcode=0;
  rcode_size=0;
  clear();
  outbuf.resize(1<<14);
  bufptr=0;
}

ZPAQL::~ZPAQL() {
  allocx(rcode, rcode_size, 0);
}

// Initialize machine state as HCOMP
void ZPAQL::inith() {
  init(header[2], header[3]); // hh, hm
}

// Initialize machine state as PCOMP
void ZPAQL::initp() {
  init(header[4], header[5]); // ph, pm
}

// Flush pending output
void ZPAQL::flush() {
  if (output) output->write(&outbuf[0], bufptr);
  if (sha1) sha1->write(&outbuf[0], bufptr);
  bufptr=0;
}

// pow(2, x)
static double pow2(int x) {
  double r=1;
  for (; x>0; x--) r+=r;
  return r;
}

// Return memory requirement in bytes
double ZPAQL::memory() {
  double mem=pow2(header[2]+2)+pow2(header[3])  // hh hm
            +pow2(header[4]+2)+pow2(header[5])  // ph pm
            +header.size();
  int cp=7;  // start of comp list
  for (int i=0; i<header[6]; ++i) {  // n
    double size=pow2(header[cp+1]); // sizebits
    switch(header[cp]) {
      case CM: mem+=4*size; break;
      case ICM: mem+=64*size+1024; break;
      case MATCH: mem+=4*size+pow2(header[cp+2]); break; // bufbits
      case MIX2: mem+=2*size; break;
      case MIX: mem+=4*size*header[cp+3]; break; // m
      case ISSE: mem+=64*size+2048; break;
      case SSE: mem+=128*size; break;
    }
    cp+=compsize[header[cp]];
  }
  return mem;
}

// Initialize machine state to run a program.
void ZPAQL::init(int hbits, int mbits) {
  if (hbits>32) error("H too big");
  if (mbits>32) error("M too big");
  h.resize(1, hbits);
  m.resize(1, mbits);
  r.resize(256);
  a=b=c=d=pc=f=0;
}

// Run program on input by interpreting header
void ZPAQL::run0(U32 input) {
  pc=hbegin;
  a=input;
  while (execute()) ;
}

// Execute one instruction, return 0 after HALT else 1
int ZPAQL::execute() {
  switch(header[pc++]) {
    case 0: err(); break; // ERROR
    case 1: ++a; break; // A++
    case 2: --a; break; // A--
    case 3: a = ~a; break; // A!
    case 4: a = 0; break; // A=0
    case 7: a = r[header[pc++]]; break; // A=R N
    case 8: swap(b); break; // B<>A
    case 9: ++b; break; // B++
    case 10: --b; break; // B--
    case 11: b = ~b; break; // B!
    case 12: b = 0; break; // B=0
    case 15: b = r[header[pc++]]; break; // B=R N
    case 16: swap(c); break; // C<>A
    case 17: ++c; break; // C++
    case 18: --c; break; // C--
    case 19: c = ~c; break; // C!
    case 20: c = 0; break; // C=0
    case 23: c = r[header[pc++]]; break; // C=R N
    case 24: swap(d); break; // D<>A
    case 25: ++d; break; // D++
    case 26: --d; break; // D--
    case 27: d = ~d; break; // D!
    case 28: d = 0; break; // D=0
    case 31: d = r[header[pc++]]; break; // D=R N
    case 32: swap(m(b)); break; // *B<>A
    case 33: ++m(b); break; // *B++
    case 34: --m(b); break; // *B--
    case 35: m(b) = ~m(b); break; // *B!
    case 36: m(b) = 0; break; // *B=0
    case 39: if (f) pc+=((header[pc]+128)&255)-127; else ++pc; break; // JT N
    case 40: swap(m(c)); break; // *C<>A
    case 41: ++m(c); break; // *C++
    case 42: --m(c); break; // *C--
    case 43: m(c) = ~m(c); break; // *C!
    case 44: m(c) = 0; break; // *C=0
    case 47: if (!f) pc+=((header[pc]+128)&255)-127; else ++pc; break; // JF N
    case 48: swap(h(d)); break; // *D<>A
    case 49: ++h(d); break; // *D++
    case 50: --h(d); break; // *D--
    case 51: h(d) = ~h(d); break; // *D!
    case 52: h(d) = 0; break; // *D=0
    case 55: r[header[pc++]] = a; break; // R=A N
    case 56: return 0  ; // HALT
    case 57: outc(a&255); break; // OUT
    case 59: a = (a+m(b)+512)*773; break; // HASH
    case 60: h(d) = (h(d)+a+512)*773; break; // HASHD
    case 63: pc+=((header[pc]+128)&255)-127; break; // JMP N
    case 64: break; // A=A
    case 65: a = b; break; // A=B
    case 66: a = c; break; // A=C
    case 67: a = d; break; // A=D
    case 68: a = m(b); break; // A=*B
    case 69: a = m(c); break; // A=*C
    case 70: a = h(d); break; // A=*D
    case 71: a = header[pc++]; break; // A= N
    case 72: b = a; break; // B=A
    case 73: break; // B=B
    case 74: b = c; break; // B=C
    case 75: b = d; break; // B=D
    case 76: b = m(b); break; // B=*B
    case 77: b = m(c); break; // B=*C
    case 78: b = h(d); break; // B=*D
    case 79: b = header[pc++]; break; // B= N
    case 80: c = a; break; // C=A
    case 81: c = b; break; // C=B
    case 82: break; // C=C
    case 83: c = d; break; // C=D
    case 84: c = m(b); break; // C=*B
    case 85: c = m(c); break; // C=*C
    case 86: c = h(d); break; // C=*D
    case 87: c = header[pc++]; break; // C= N
    case 88: d = a; break; // D=A
    case 89: d = b; break; // D=B
    case 90: d = c; break; // D=C
    case 91: break; // D=D
    case 92: d = m(b); break; // D=*B
    case 93: d = m(c); break; // D=*C
    case 94: d = h(d); break; // D=*D
    case 95: d = header[pc++]; break; // D= N
    case 96: m(b) = a; break; // *B=A
    case 97: m(b) = b; break; // *B=B
    case 98: m(b) = c; break; // *B=C
    case 99: m(b) = d; break; // *B=D
    case 100: break; // *B=*B
    case 101: m(b) = m(c); break; // *B=*C
    case 102: m(b) = h(d); break; // *B=*D
    case 103: m(b) = header[pc++]; break; // *B= N
    case 104: m(c) = a; break; // *C=A
    case 105: m(c) = b; break; // *C=B
    case 106: m(c) = c; break; // *C=C
    case 107: m(c) = d; break; // *C=D
    case 108: m(c) = m(b); break; // *C=*B
    case 109: break; // *C=*C
    case 110: m(c) = h(d); break; // *C=*D
    case 111: m(c) = header[pc++]; break; // *C= N
    case 112: h(d) = a; break; // *D=A
    case 113: h(d) = b; break; // *D=B
    case 114: h(d) = c; break; // *D=C
    case 115: h(d) = d; break; // *D=D
    case 116: h(d) = m(b); break; // *D=*B
    case 117: h(d) = m(c); break; // *D=*C
    case 118: break; // *D=*D
    case 119: h(d) = header[pc++]; break; // *D= N
    case 128: a += a; break; // A+=A
    case 129: a += b; break; // A+=B
    case 130: a += c; break; // A+=C
    case 131: a += d; break; // A+=D
    case 132: a += m(b); break; // A+=*B
    case 133: a += m(c); break; // A+=*C
    case 134: a += h(d); break; // A+=*D
    case 135: a += header[pc++]; break; // A+= N
    case 136: a -= a; break; // A-=A
    case 137: a -= b; break; // A-=B
    case 138: a -= c; break; // A-=C
    case 139: a -= d; break; // A-=D
    case 140: a -= m(b); break; // A-=*B
    case 141: a -= m(c); break; // A-=*C
    case 142: a -= h(d); break; // A-=*D
    case 143: a -= header[pc++]; break; // A-= N
    case 144: a *= a; break; // A*=A
    case 145: a *= b; break; // A*=B
    case 146: a *= c; break; // A*=C
    case 147: a *= d; break; // A*=D
    case 148: a *= m(b); break; // A*=*B
    case 149: a *= m(c); break; // A*=*C
    case 150: a *= h(d); break; // A*=*D
    case 151: a *= header[pc++]; break; // A*= N
    case 152: div(a); break; // A/=A
    case 153: div(b); break; // A/=B
    case 154: div(c); break; // A/=C
    case 155: div(d); break; // A/=D
    case 156: div(m(b)); break; // A/=*B
    case 157: div(m(c)); break; // A/=*C
    case 158: div(h(d)); break; // A/=*D
    case 159: div(header[pc++]); break; // A/= N
    case 160: mod(a); break; // A%=A
    case 161: mod(b); break; // A%=B
    case 162: mod(c); break; // A%=C
    case 163: mod(d); break; // A%=D
    case 164: mod(m(b)); break; // A%=*B
    case 165: mod(m(c)); break; // A%=*C
    case 166: mod(h(d)); break; // A%=*D
    case 167: mod(header[pc++]); break; // A%= N
    case 168: a &= a; break; // A&=A
    case 169: a &= b; break; // A&=B
    case 170: a &= c; break; // A&=C
    case 171: a &= d; break; // A&=D
    case 172: a &= m(b); break; // A&=*B
    case 173: a &= m(c); break; // A&=*C
    case 174: a &= h(d); break; // A&=*D
    case 175: a &= header[pc++]; break; // A&= N
    case 176: a &= ~ a; break; // A&~A
    case 177: a &= ~ b; break; // A&~B
    case 178: a &= ~ c; break; // A&~C
    case 179: a &= ~ d; break; // A&~D
    case 180: a &= ~ m(b); break; // A&~*B
    case 181: a &= ~ m(c); break; // A&~*C
    case 182: a &= ~ h(d); break; // A&~*D
    case 183: a &= ~ header[pc++]; break; // A&~ N
    case 184: a |= a; break; // A|=A
    case 185: a |= b; break; // A|=B
    case 186: a |= c; break; // A|=C
    case 187: a |= d; break; // A|=D
    case 188: a |= m(b); break; // A|=*B
    case 189: a |= m(c); break; // A|=*C
    case 190: a |= h(d); break; // A|=*D
    case 191: a |= header[pc++]; break; // A|= N
    case 192: a ^= a; break; // A^=A
    case 193: a ^= b; break; // A^=B
    case 194: a ^= c; break; // A^=C
    case 195: a ^= d; break; // A^=D
    case 196: a ^= m(b); break; // A^=*B
    case 197: a ^= m(c); break; // A^=*C
    case 198: a ^= h(d); break; // A^=*D
    case 199: a ^= header[pc++]; break; // A^= N
    case 200: a <<= (a&31); break; // A<<=A
    case 201: a <<= (b&31); break; // A<<=B
    case 202: a <<= (c&31); break; // A<<=C
    case 203: a <<= (d&31); break; // A<<=D
    case 204: a <<= (m(b)&31); break; // A<<=*B
    case 205: a <<= (m(c)&31); break; // A<<=*C
    case 206: a <<= (h(d)&31); break; // A<<=*D
    case 207: a <<= (header[pc++]&31); break; // A<<= N
    case 208: a >>= (a&31); break; // A>>=A
    case 209: a >>= (b&31); break; // A>>=B
    case 210: a >>= (c&31); break; // A>>=C
    case 211: a >>= (d&31); break; // A>>=D
    case 212: a >>= (m(b)&31); break; // A>>=*B
    case 213: a >>= (m(c)&31); break; // A>>=*C
    case 214: a >>= (h(d)&31); break; // A>>=*D
    case 215: a >>= (header[pc++]&31); break; // A>>= N
    case 216: f = 1; break; // A==A
    case 217: f = (a == b); break; // A==B
    case 218: f = (a == c); break; // A==C
    case 219: f = (a == d); break; // A==D
    case 220: f = (a == U32(m(b))); break; // A==*B
    case 221: f = (a == U32(m(c))); break; // A==*C
    case 222: f = (a == h(d)); break; // A==*D
    case 223: f = (a == U32(header[pc++])); break; // A== N
    case 224: f = 0; break; // A<A
    case 225: f = (a < b); break; // A<B
    case 226: f = (a < c); break; // A<C
    case 227: f = (a < d); break; // A<D
    case 228: f = (a < U32(m(b))); break; // A<*B
    case 229: f = (a < U32(m(c))); break; // A<*C
    case 230: f = (a < h(d)); break; // A<*D
    case 231: f = (a < U32(header[pc++])); break; // A< N
    case 232: f = 0; break; // A>A
    case 233: f = (a > b); break; // A>B
    case 234: f = (a > c); break; // A>C
    case 235: f = (a > d); break; // A>D
    case 236: f = (a > U32(m(b))); break; // A>*B
    case 237: f = (a > U32(m(c))); break; // A>*C
    case 238: f = (a > h(d)); break; // A>*D
    case 239: f = (a > U32(header[pc++])); break; // A> N
    case 255: if((pc=hbegin+header[pc]+256*header[pc+1])>=hend)err();break;//LJ
    default: err();
  }
  return 1;
}

// Print illegal instruction error message and exit
void ZPAQL::err() {
  error("ZPAQL execution error");
}

///////////////////////// Predictor /////////////////////////

// sdt2k[i]=2048/i;
static const int sdt2k[256]={
     0,  2048,  1024,   682,   512,   409,   341,   292,
   256,   227,   204,   186,   170,   157,   146,   136,
   128,   120,   113,   107,   102,    97,    93,    89,
    85,    81,    78,    75,    73,    70,    68,    66,
    64,    62,    60,    58,    56,    55,    53,    52,
    51,    49,    48,    47,    46,    45,    44,    43,
    42,    41,    40,    40,    39,    38,    37,    37,
    36,    35,    35,    34,    34,    33,    33,    32,
    32,    31,    31,    30,    30,    29,    29,    28,
    28,    28,    27,    27,    26,    26,    26,    25,
    25,    25,    24,    24,    24,    24,    23,    23,
    23,    23,    22,    22,    22,    22,    21,    21,
    21,    21,    20,    20,    20,    20,    20,    19,
    19,    19,    19,    19,    18,    18,    18,    18,
    18,    18,    17,    17,    17,    17,    17,    17,
    17,    16,    16,    16,    16,    16,    16,    16,
    16,    15,    15,    15,    15,    15,    15,    15,
    15,    14,    14,    14,    14,    14,    14,    14,
    14,    14,    14,    13,    13,    13,    13,    13,
    13,    13,    13,    13,    13,    13,    12,    12,
    12,    12,    12,    12,    12,    12,    12,    12,
    12,    12,    12,    11,    11,    11,    11,    11,
    11,    11,    11,    11,    11,    11,    11,    11,
    11,    11,    11,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,     9,     9,     9,
     9,     9,     9,     9,     9,     9,     9,     9,
     9,     9,     9,     9,     9,     9,     9,     9,
     9,     9,     9,     9,     8,     8,     8,     8,
     8,     8,     8,     8,     8,     8,     8,     8,
     8,     8,     8,     8,     8,     8,     8,     8,
     8,     8,     8,     8,     8,     8,     8,     8
};

// sdt[i]=(1<<17)/(i*2+3)*2;
static const int sdt[1024]={
 87380, 52428, 37448, 29126, 23830, 20164, 17476, 15420,
 13796, 12482, 11396, 10484,  9708,  9038,  8456,  7942,
  7488,  7084,  6720,  6392,  6096,  5824,  5576,  5348,
  5140,  4946,  4766,  4598,  4442,  4296,  4160,  4032,
  3912,  3798,  3692,  3590,  3494,  3404,  3318,  3236,
  3158,  3084,  3012,  2944,  2880,  2818,  2758,  2702,
  2646,  2594,  2544,  2496,  2448,  2404,  2360,  2318,
  2278,  2240,  2202,  2166,  2130,  2096,  2064,  2032,
  2000,  1970,  1940,  1912,  1884,  1858,  1832,  1806,
  1782,  1758,  1736,  1712,  1690,  1668,  1648,  1628,
  1608,  1588,  1568,  1550,  1532,  1514,  1496,  1480,
  1464,  1448,  1432,  1416,  1400,  1386,  1372,  1358,
  1344,  1330,  1316,  1304,  1290,  1278,  1266,  1254,
  1242,  1230,  1218,  1208,  1196,  1186,  1174,  1164,
  1154,  1144,  1134,  1124,  1114,  1106,  1096,  1086,
  1078,  1068,  1060,  1052,  1044,  1036,  1028,  1020,
  1012,  1004,   996,   988,   980,   974,   966,   960,
   952,   946,   938,   932,   926,   918,   912,   906,
   900,   894,   888,   882,   876,   870,   864,   858,
   852,   848,   842,   836,   832,   826,   820,   816,
   810,   806,   800,   796,   790,   786,   782,   776,
   772,   768,   764,   758,   754,   750,   746,   742,
   738,   734,   730,   726,   722,   718,   714,   710,
   706,   702,   698,   694,   690,   688,   684,   680,
   676,   672,   670,   666,   662,   660,   656,   652,
   650,   646,   644,   640,   636,   634,   630,   628,
   624,   622,   618,   616,   612,   610,   608,   604,
   602,   598,   596,   594,   590,   588,   586,   582,
   580,   578,   576,   572,   570,   568,   566,   562,
   560,   558,   556,   554,   550,   548,   546,   544,
   542,   540,   538,   536,   532,   530,   528,   526,
   524,   522,   520,   518,   516,   514,   512,   510,
   508,   506,   504,   502,   500,   498,   496,   494,
   492,   490,   488,   488,   486,   484,   482,   480,
   478,   476,   474,   474,   472,   470,   468,   466,
   464,   462,   462,   460,   458,   456,   454,   454,
   452,   450,   448,   448,   446,   444,   442,   442,
   440,   438,   436,   436,   434,   432,   430,   430,
   428,   426,   426,   424,   422,   422,   420,   418,
   418,   416,   414,   414,   412,   410,   410,   408,
   406,   406,   404,   402,   402,   400,   400,   398,
   396,   396,   394,   394,   392,   390,   390,   388,
   388,   386,   386,   384,   382,   382,   380,   380,
   378,   378,   376,   376,   374,   372,   372,   370,
   370,   368,   368,   366,   366,   364,   364,   362,
   362,   360,   360,   358,   358,   356,   356,   354,
   354,   352,   352,   350,   350,   348,   348,   348,
   346,   346,   344,   344,   342,   342,   340,   340,
   340,   338,   338,   336,   336,   334,   334,   332,
   332,   332,   330,   330,   328,   328,   328,   326,
   326,   324,   324,   324,   322,   322,   320,   320,
   320,   318,   318,   316,   316,   316,   314,   314,
   312,   312,   312,   310,   310,   310,   308,   308,
   308,   306,   306,   304,   304,   304,   302,   302,
   302,   300,   300,   300,   298,   298,   298,   296,
   296,   296,   294,   294,   294,   292,   292,   292,
   290,   290,   290,   288,   288,   288,   286,   286,
   286,   284,   284,   284,   284,   282,   282,   282,
   280,   280,   280,   278,   278,   278,   276,   276,
   276,   276,   274,   274,   274,   272,   272,   272,
   272,   270,   270,   270,   268,   268,   268,   268,
   266,   266,   266,   266,   264,   264,   264,   262,
   262,   262,   262,   260,   260,   260,   260,   258,
   258,   258,   258,   256,   256,   256,   256,   254,
   254,   254,   254,   252,   252,   252,   252,   250,
   250,   250,   250,   248,   248,   248,   248,   248,
   246,   246,   246,   246,   244,   244,   244,   244,
   242,   242,   242,   242,   242,   240,   240,   240,
   240,   238,   238,   238,   238,   238,   236,   236,
   236,   236,   234,   234,   234,   234,   234,   232,
   232,   232,   232,   232,   230,   230,   230,   230,
   230,   228,   228,   228,   228,   228,   226,   226,
   226,   226,   226,   224,   224,   224,   224,   224,
   222,   222,   222,   222,   222,   220,   220,   220,
   220,   220,   220,   218,   218,   218,   218,   218,
   216,   216,   216,   216,   216,   216,   214,   214,
   214,   214,   214,   212,   212,   212,   212,   212,
   212,   210,   210,   210,   210,   210,   210,   208,
   208,   208,   208,   208,   208,   206,   206,   206,
   206,   206,   206,   204,   204,   204,   204,   204,
   204,   204,   202,   202,   202,   202,   202,   202,
   200,   200,   200,   200,   200,   200,   198,   198,
   198,   198,   198,   198,   198,   196,   196,   196,
   196,   196,   196,   196,   194,   194,   194,   194,
   194,   194,   194,   192,   192,   192,   192,   192,
   192,   192,   190,   190,   190,   190,   190,   190,
   190,   188,   188,   188,   188,   188,   188,   188,
   186,   186,   186,   186,   186,   186,   186,   186,
   184,   184,   184,   184,   184,   184,   184,   182,
   182,   182,   182,   182,   182,   182,   182,   180,
   180,   180,   180,   180,   180,   180,   180,   178,
   178,   178,   178,   178,   178,   178,   178,   176,
   176,   176,   176,   176,   176,   176,   176,   176,
   174,   174,   174,   174,   174,   174,   174,   174,
   172,   172,   172,   172,   172,   172,   172,   172,
   172,   170,   170,   170,   170,   170,   170,   170,
   170,   170,   168,   168,   168,   168,   168,   168,
   168,   168,   168,   166,   166,   166,   166,   166,
   166,   166,   166,   166,   166,   164,   164,   164,
   164,   164,   164,   164,   164,   164,   162,   162,
   162,   162,   162,   162,   162,   162,   162,   162,
   160,   160,   160,   160,   160,   160,   160,   160,
   160,   160,   158,   158,   158,   158,   158,   158,
   158,   158,   158,   158,   158,   156,   156,   156,
   156,   156,   156,   156,   156,   156,   156,   154,
   154,   154,   154,   154,   154,   154,   154,   154,
   154,   154,   152,   152,   152,   152,   152,   152,
   152,   152,   152,   152,   152,   150,   150,   150,
   150,   150,   150,   150,   150,   150,   150,   150,
   150,   148,   148,   148,   148,   148,   148,   148,
   148,   148,   148,   148,   148,   146,   146,   146,
   146,   146,   146,   146,   146,   146,   146,   146,
   146,   144,   144,   144,   144,   144,   144,   144,
   144,   144,   144,   144,   144,   142,   142,   142,
   142,   142,   142,   142,   142,   142,   142,   142,
   142,   142,   140,   140,   140,   140,   140,   140,
   140,   140,   140,   140,   140,   140,   140,   138,
   138,   138,   138,   138,   138,   138,   138,   138,
   138,   138,   138,   138,   138,   136,   136,   136,
   136,   136,   136,   136,   136,   136,   136,   136,
   136,   136,   136,   134,   134,   134,   134,   134,
   134,   134,   134,   134,   134,   134,   134,   134,
   134,   132,   132,   132,   132,   132,   132,   132,
   132,   132,   132,   132,   132,   132,   132,   132,
   130,   130,   130,   130,   130,   130,   130,   130,
   130,   130,   130,   130,   130,   130,   130,   128,
   128,   128,   128,   128,   128,   128,   128,   128,
   128,   128,   128,   128,   128,   128,   128,   126
};

// ssquasht[i]=int(32768.0/(1+exp((i-2048)*(-1.0/64))));
// Middle 1344 of 4096 entries only.
static const U16 ssquasht[1344]={
     0,     0,     0,     0,     0,     0,     0,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,
     4,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     4,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     5,
     5,     5,     6,     6,     6,     6,     6,     6,
     6,     6,     6,     6,     7,     7,     7,     7,
     7,     7,     7,     7,     8,     8,     8,     8,
     8,     8,     8,     8,     9,     9,     9,     9,
     9,     9,    10,    10,    10,    10,    10,    10,
    10,    11,    11,    11,    11,    11,    12,    12,
    12,    12,    12,    13,    13,    13,    13,    13,
    14,    14,    14,    14,    15,    15,    15,    15,
    15,    16,    16,    16,    17,    17,    17,    17,
    18,    18,    18,    18,    19,    19,    19,    20,
    20,    20,    21,    21,    21,    22,    22,    22,
    23,    23,    23,    24,    24,    25,    25,    25,
    26,    26,    27,    27,    28,    28,    28,    29,
    29,    30,    30,    31,    31,    32,    32,    33,
    33,    34,    34,    35,    36,    36,    37,    37,
    38,    38,    39,    40,    40,    41,    42,    42,
    43,    44,    44,    45,    46,    46,    47,    48,
    49,    49,    50,    51,    52,    53,    54,    54,
    55,    56,    57,    58,    59,    60,    61,    62,
    63,    64,    65,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    76,    77,    78,    79,
    81,    82,    83,    84,    86,    87,    88,    90,
    91,    93,    94,    96,    97,    99,   100,   102,
   103,   105,   107,   108,   110,   112,   114,   115,
   117,   119,   121,   123,   125,   127,   129,   131,
   133,   135,   137,   139,   141,   144,   146,   148,
   151,   153,   155,   158,   160,   163,   165,   168,
   171,   173,   176,   179,   182,   184,   187,   190,
   193,   196,   199,   202,   206,   209,   212,   215,
   219,   222,   226,   229,   233,   237,   240,   244,
   248,   252,   256,   260,   264,   268,   272,   276,
   281,   285,   289,   294,   299,   303,   308,   313,
   318,   323,   328,   333,   338,   343,   349,   354,
   360,   365,   371,   377,   382,   388,   394,   401,
   407,   413,   420,   426,   433,   440,   446,   453,
   460,   467,   475,   482,   490,   497,   505,   513,
   521,   529,   537,   545,   554,   562,   571,   580,
   589,   598,   607,   617,   626,   636,   646,   656,
   666,   676,   686,   697,   708,   719,   730,   741,
   752,   764,   776,   788,   800,   812,   825,   837,
   850,   863,   876,   890,   903,   917,   931,   946,
   960,   975,   990,  1005,  1020,  1036,  1051,  1067,
  1084,  1100,  1117,  1134,  1151,  1169,  1186,  1204,
  1223,  1241,  1260,  1279,  1298,  1318,  1338,  1358,
  1379,  1399,  1421,  1442,  1464,  1486,  1508,  1531,
  1554,  1577,  1600,  1624,  1649,  1673,  1698,  1724,
  1749,  1775,  1802,  1829,  1856,  1883,  1911,  1940,
  1968,  1998,  2027,  2057,  2087,  2118,  2149,  2181,
  2213,  2245,  2278,  2312,  2345,  2380,  2414,  2450,
  2485,  2521,  2558,  2595,  2633,  2671,  2709,  2748,
  2788,  2828,  2869,  2910,  2952,  2994,  3037,  3080,
  3124,  3168,  3213,  3259,  3305,  3352,  3399,  3447,
  3496,  3545,  3594,  3645,  3696,  3747,  3799,  3852,
  3906,  3960,  4014,  4070,  4126,  4182,  4240,  4298,
  4356,  4416,  4476,  4537,  4598,  4660,  4723,  4786,
  4851,  4916,  4981,  5048,  5115,  5183,  5251,  5320,
  5390,  5461,  5533,  5605,  5678,  5752,  5826,  5901,
  5977,  6054,  6131,  6210,  6289,  6369,  6449,  6530,
  6613,  6695,  6779,  6863,  6949,  7035,  7121,  7209,
  7297,  7386,  7476,  7566,  7658,  7750,  7842,  7936,
  8030,  8126,  8221,  8318,  8415,  8513,  8612,  8712,
  8812,  8913,  9015,  9117,  9221,  9324,  9429,  9534,
  9640,  9747,  9854,  9962, 10071, 10180, 10290, 10401,
 10512, 10624, 10737, 10850, 10963, 11078, 11192, 11308,
 11424, 11540, 11658, 11775, 11893, 12012, 12131, 12251,
 12371, 12491, 12612, 12734, 12856, 12978, 13101, 13224,
 13347, 13471, 13595, 13719, 13844, 13969, 14095, 14220,
 14346, 14472, 14599, 14725, 14852, 14979, 15106, 15233,
 15361, 15488, 15616, 15744, 15872, 16000, 16128, 16256,
 16384, 16511, 16639, 16767, 16895, 17023, 17151, 17279,
 17406, 17534, 17661, 17788, 17915, 18042, 18168, 18295,
 18421, 18547, 18672, 18798, 18923, 19048, 19172, 19296,
 19420, 19543, 19666, 19789, 19911, 20033, 20155, 20276,
 20396, 20516, 20636, 20755, 20874, 20992, 21109, 21227,
 21343, 21459, 21575, 21689, 21804, 21917, 22030, 22143,
 22255, 22366, 22477, 22587, 22696, 22805, 22913, 23020,
 23127, 23233, 23338, 23443, 23546, 23650, 23752, 23854,
 23955, 24055, 24155, 24254, 24352, 24449, 24546, 24641,
 24737, 24831, 24925, 25017, 25109, 25201, 25291, 25381,
 25470, 25558, 25646, 25732, 25818, 25904, 25988, 26072,
 26154, 26237, 26318, 26398, 26478, 26557, 26636, 26713,
 26790, 26866, 26941, 27015, 27089, 27162, 27234, 27306,
 27377, 27447, 27516, 27584, 27652, 27719, 27786, 27851,
 27916, 27981, 28044, 28107, 28169, 28230, 28291, 28351,
 28411, 28469, 28527, 28585, 28641, 28697, 28753, 28807,
 28861, 28915, 28968, 29020, 29071, 29122, 29173, 29222,
 29271, 29320, 29368, 29415, 29462, 29508, 29554, 29599,
 29643, 29687, 29730, 29773, 29815, 29857, 29898, 29939,
 29979, 30019, 30058, 30096, 30134, 30172, 30209, 30246,
 30282, 30317, 30353, 30387, 30422, 30455, 30489, 30522,
 30554, 30586, 30618, 30649, 30680, 30710, 30740, 30769,
 30799, 30827, 30856, 30884, 30911, 30938, 30965, 30992,
 31018, 31043, 31069, 31094, 31118, 31143, 31167, 31190,
 31213, 31236, 31259, 31281, 31303, 31325, 31346, 31368,
 31388, 31409, 31429, 31449, 31469, 31488, 31507, 31526,
 31544, 31563, 31581, 31598, 31616, 31633, 31650, 31667,
 31683, 31700, 31716, 31731, 31747, 31762, 31777, 31792,
 31807, 31821, 31836, 31850, 31864, 31877, 31891, 31904,
 31917, 31930, 31942, 31955, 31967, 31979, 31991, 32003,
 32015, 32026, 32037, 32048, 32059, 32070, 32081, 32091,
 32101, 32111, 32121, 32131, 32141, 32150, 32160, 32169,
 32178, 32187, 32196, 32205, 32213, 32222, 32230, 32238,
 32246, 32254, 32262, 32270, 32277, 32285, 32292, 32300,
 32307, 32314, 32321, 32327, 32334, 32341, 32347, 32354,
 32360, 32366, 32373, 32379, 32385, 32390, 32396, 32402,
 32407, 32413, 32418, 32424, 32429, 32434, 32439, 32444,
 32449, 32454, 32459, 32464, 32468, 32473, 32478, 32482,
 32486, 32491, 32495, 32499, 32503, 32507, 32511, 32515,
 32519, 32523, 32527, 32530, 32534, 32538, 32541, 32545,
 32548, 32552, 32555, 32558, 32561, 32565, 32568, 32571,
 32574, 32577, 32580, 32583, 32585, 32588, 32591, 32594,
 32596, 32599, 32602, 32604, 32607, 32609, 32612, 32614,
 32616, 32619, 32621, 32623, 32626, 32628, 32630, 32632,
 32634, 32636, 32638, 32640, 32642, 32644, 32646, 32648,
 32650, 32652, 32653, 32655, 32657, 32659, 32660, 32662,
 32664, 32665, 32667, 32668, 32670, 32671, 32673, 32674,
 32676, 32677, 32679, 32680, 32681, 32683, 32684, 32685,
 32686, 32688, 32689, 32690, 32691, 32693, 32694, 32695,
 32696, 32697, 32698, 32699, 32700, 32701, 32702, 32703,
 32704, 32705, 32706, 32707, 32708, 32709, 32710, 32711,
 32712, 32713, 32713, 32714, 32715, 32716, 32717, 32718,
 32718, 32719, 32720, 32721, 32721, 32722, 32723, 32723,
 32724, 32725, 32725, 32726, 32727, 32727, 32728, 32729,
 32729, 32730, 32730, 32731, 32731, 32732, 32733, 32733,
 32734, 32734, 32735, 32735, 32736, 32736, 32737, 32737,
 32738, 32738, 32739, 32739, 32739, 32740, 32740, 32741,
 32741, 32742, 32742, 32742, 32743, 32743, 32744, 32744,
 32744, 32745, 32745, 32745, 32746, 32746, 32746, 32747,
 32747, 32747, 32748, 32748, 32748, 32749, 32749, 32749,
 32749, 32750, 32750, 32750, 32750, 32751, 32751, 32751,
 32752, 32752, 32752, 32752, 32752, 32753, 32753, 32753,
 32753, 32754, 32754, 32754, 32754, 32754, 32755, 32755,
 32755, 32755, 32755, 32756, 32756, 32756, 32756, 32756,
 32757, 32757, 32757, 32757, 32757, 32757, 32757, 32758,
 32758, 32758, 32758, 32758, 32758, 32759, 32759, 32759,
 32759, 32759, 32759, 32759, 32759, 32760, 32760, 32760,
 32760, 32760, 32760, 32760, 32760, 32761, 32761, 32761,
 32761, 32761, 32761, 32761, 32761, 32761, 32761, 32762,
 32762, 32762, 32762, 32762, 32762, 32762, 32762, 32762,
 32762, 32762, 32762, 32763, 32763, 32763, 32763, 32763,
 32763, 32763, 32763, 32763, 32763, 32763, 32763, 32763,
 32763, 32764, 32764, 32764, 32764, 32764, 32764, 32764,
 32764, 32764, 32764, 32764, 32764, 32764, 32764, 32764,
 32764, 32764, 32764, 32764, 32765, 32765, 32765, 32765,
 32765, 32765, 32765, 32765, 32765, 32765, 32765, 32765,
 32765, 32765, 32765, 32765, 32765, 32765, 32765, 32765,
 32765, 32765, 32765, 32765, 32765, 32765, 32766, 32766,
 32766, 32766, 32766, 32766, 32766, 32766, 32766, 32766,
 32766, 32766, 32766, 32766, 32766, 32766, 32766, 32766,
 32766, 32766, 32766, 32766, 32766, 32766, 32766, 32766,
 32766, 32766, 32766, 32766, 32766, 32766, 32766, 32766,
 32766, 32766, 32766, 32766, 32766, 32766, 32766, 32766,
 32766, 32766, 32767, 32767, 32767, 32767, 32767, 32767
};

// stdt[i]=count of -i or i in botton or top of stretcht[]
static const U8 stdt[712]={
    64,   128,   128,   128,   128,   128,   127,   128,
   127,   128,   127,   127,   127,   127,   126,   126,
   126,   126,   126,   125,   125,   124,   125,   124,
   123,   123,   123,   123,   122,   122,   121,   121,
   120,   120,   119,   119,   118,   118,   118,   116,
   117,   115,   116,   114,   114,   113,   113,   112,
   112,   111,   110,   110,   109,   108,   108,   107,
   106,   106,   105,   104,   104,   102,   103,   101,
   101,   100,    99,    98,    98,    97,    96,    96,
    94,    94,    94,    92,    92,    91,    90,    89,
    89,    88,    87,    86,    86,    84,    84,    84,
    82,    82,    81,    80,    79,    79,    78,    77,
    76,    76,    75,    74,    73,    73,    72,    71,
    70,    70,    69,    68,    67,    67,    66,    65,
    65,    64,    63,    62,    62,    61,    61,    59,
    59,    59,    57,    58,    56,    56,    55,    54,
    54,    53,    52,    52,    51,    51,    50,    49,
    49,    48,    48,    47,    47,    45,    46,    44,
    45,    43,    43,    43,    42,    41,    41,    40,
    40,    40,    39,    38,    38,    37,    37,    36,
    36,    36,    35,    34,    34,    34,    33,    32,
    33,    32,    31,    31,    30,    31,    29,    30,
    28,    29,    28,    28,    27,    27,    27,    26,
    26,    25,    26,    24,    25,    24,    24,    23,
    23,    23,    23,    22,    22,    21,    22,    21,
    20,    21,    20,    19,    20,    19,    19,    19,
    18,    18,    18,    18,    17,    17,    17,    17,
    16,    16,    16,    16,    15,    15,    15,    15,
    15,    14,    14,    14,    14,    13,    14,    13,
    13,    13,    12,    13,    12,    12,    12,    11,
    12,    11,    11,    11,    11,    11,    10,    11,
    10,    10,    10,    10,     9,    10,     9,     9,
     9,     9,     9,     8,     9,     8,     9,     8,
     8,     8,     7,     8,     8,     7,     7,     8,
     7,     7,     7,     6,     7,     7,     6,     6,
     7,     6,     6,     6,     6,     6,     6,     5,
     6,     5,     6,     5,     5,     5,     5,     5,
     5,     5,     5,     5,     4,     5,     4,     5,
     4,     4,     5,     4,     4,     4,     4,     4,
     4,     3,     4,     4,     3,     4,     4,     3,
     3,     4,     3,     3,     3,     4,     3,     3,
     3,     3,     3,     3,     2,     3,     3,     3,
     2,     3,     2,     3,     3,     2,     2,     3,
     2,     2,     3,     2,     2,     2,     2,     3,
     2,     2,     2,     2,     2,     2,     1,     2,
     2,     2,     2,     1,     2,     2,     2,     1,
     2,     1,     2,     2,     1,     2,     1,     2,
     1,     1,     2,     1,     1,     2,     1,     1,
     2,     1,     1,     1,     1,     2,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     0,     1,     1,     1,     1,     0,
     1,     1,     1,     0,     1,     1,     1,     0,
     1,     1,     0,     1,     1,     0,     1,     0,
     1,     1,     0,     1,     0,     1,     0,     1,
     0,     1,     0,     1,     0,     1,     0,     1,
     0,     1,     0,     1,     0,     1,     0,     0,
     1,     0,     1,     0,     0,     1,     0,     1,
     0,     0,     1,     0,     0,     1,     0,     0,
     1,     0,     0,     1,     0,     0,     0,     1,
     0,     0,     1,     0,     0,     0,     1,     0,
     0,     0,     1,     0,     0,     0,     1,     0,
     0,     0,     0,     1,     0,     0,     0,     0,
     1,     0,     0,     0,     0,     1,     0,     0,
     0,     0,     0,     1,     0,     0,     0,     0,
     0,     1,     0,     0,     0,     0,     0,     0,
     1,     0,     0,     0,     0,     0,     0,     0,
     1,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     1,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     1,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     1,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     1,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     1,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     1,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     1,     0
};

Predictor::Predictor(ZPAQL& zr):
    c8(1), hmap4(1), z(zr) {
  pcode=0;
  pcode_size=0;
  initTables=false;
}

Predictor::~Predictor() {
  allocx(pcode, pcode_size, 0);  // free executable memory
}

// Initialize the predictor with a new model in z
void Predictor::init() {

  // Clear old JIT code if any
  allocx(pcode, pcode_size, 0);

  // Initialize context hash function
  z.inith();

  // Initialize model independent tables
  if (!initTables && isModeled()) {
    initTables=true;
    memcpy(dt2k, sdt2k, sizeof(dt2k));
    memcpy(dt, sdt, sizeof(dt));

    // ssquasht[i]=int(32768.0/(1+exp((i-2048)*(-1.0/64))));
    // Copy middle 1344 of 4096 entries.
    memset(squasht, 0, 1376*2);
    memcpy(squasht+1376, ssquasht, 1344*2);
    for (int i=2720; i<4096; ++i) squasht[i]=32767;

    // sstretcht[i]=int(log((i+0.5)/(32767.5-i))*64+0.5+100000)-100000;
    int k=16384;
    for (int i=0; i<712; ++i)
      for (int j=stdt[i]; j>0; --j)
        stretcht[k++]=i;
    for (int i=0; i<16384; ++i)
      stretcht[i]=-stretcht[32767-i];
  }

  // Initialize predictions
  for (int i=0; i<256; ++i) h[i]=p[i]=0;

  // Initialize components
  for (int i=0; i<256; ++i)  // clear old model
    comp[i].init();
  int n=z.header[6]; // hsize[0..1] hh hm ph pm n (comp)[n] END 0[128] (hcomp) END
  const U8* cp=&z.header[7];  // start of component list
  for (int i=0; i<n; ++i) {
    Component& cr=comp[i];
    switch(cp[0]) {
      case CONS:  // c
        p[i]=(cp[1]-128)*4;
        break;
      case CM: // sizebits limit
        if (cp[1]>32) error("max size for CM is 32");
        cr.cm.resize(1, cp[1]);  // packed CM (22 bits) + CMCOUNT (10 bits)
        cr.limit=cp[2]*4;
        for (size_t j=0; j<cr.cm.size(); ++j)
          cr.cm[j]=0x80000000;
        break;
      case ICM: // sizebits
        if (cp[1]>26) error("max size for ICM is 26");
        cr.limit=1023;
        cr.cm.resize(256);
        cr.ht.resize(64, cp[1]);
        for (size_t j=0; j<cr.cm.size(); ++j)
          cr.cm[j]=st.cminit(j);
        break;
      case MATCH:  // sizebits
        if (cp[1]>32 || cp[2]>32) error("max size for MATCH is 32 32");
        cr.cm.resize(1, cp[1]);  // index
        cr.ht.resize(1, cp[2]);  // buf
        cr.ht(0)=1;
        break;
      case AVG: // j k wt
        if (cp[1]>=i) error("AVG j >= i");
        if (cp[2]>=i) error("AVG k >= i");
        break;
      case MIX2:  // sizebits j k rate mask
        if (cp[1]>32) error("max size for MIX2 is 32");
        if (cp[3]>=i) error("MIX2 k >= i");
        if (cp[2]>=i) error("MIX2 j >= i");
        cr.c=(size_t(1)<<cp[1]); // size (number of contexts)
        cr.a16.resize(1, cp[1]);  // wt[size][m]
        for (size_t j=0; j<cr.a16.size(); ++j)
          cr.a16[j]=32768;
        break;
      case MIX: {  // sizebits j m rate mask
        if (cp[1]>32) error("max size for MIX is 32");
        if (cp[2]>=i) error("MIX j >= i");
        if (cp[3]<1 || cp[3]>i-cp[2]) error("MIX m not in 1..i-j");
        int m=cp[3];  // number of inputs
        cr.c=(size_t(1)<<cp[1]); // size (number of contexts)
        cr.cm.resize(m, cp[1]);  // wt[size][m]
        for (size_t j=0; j<cr.cm.size(); ++j)
          cr.cm[j]=65536/m;
        break;
      }
      case ISSE:  // sizebits j
        if (cp[1]>32) error("max size for ISSE is 32");
        if (cp[2]>=i) error("ISSE j >= i");
        cr.ht.resize(64, cp[1]);
        cr.cm.resize(512);
        for (int j=0; j<256; ++j) {
          cr.cm[j*2]=1<<15;
          cr.cm[j*2+1]=clamp512k(stretch(st.cminit(j)>>8)*1024);
        }
        break;
      case SSE: // sizebits j start limit
        if (cp[1]>32) error("max size for SSE is 32");
        if (cp[2]>=i) error("SSE j >= i");
        if (cp[3]>cp[4]*4) error("SSE start > limit*4");
        cr.cm.resize(32, cp[1]);
        cr.limit=cp[4]*4;
        for (size_t j=0; j<cr.cm.size(); ++j)
          cr.cm[j]=squash((j&31)*64-992)<<17|cp[3];
        break;
      default: error("unknown component type");
    }
    cp+=compsize[*cp];
  }
}

// Return next bit prediction using interpreted COMP code
int Predictor::predict0() {
  // Predict next bit
  int n=z.header[6];
  const U8* cp=&z.header[7];
  for (int i=0; i<n; ++i) {
    Component& cr=comp[i];
    switch(cp[0]) {
      case CONS:  // c
        break;
      case CM:  // sizebits limit
        cr.cxt=h[i]^hmap4;
        p[i]=stretch(cr.cm(cr.cxt)>>17);
        break;
      case ICM: // sizebits
        if (c8==1 || (c8&0xf0)==16) cr.c=find(cr.ht, cp[1]+2, h[i]+16*c8);
        cr.cxt=cr.ht[cr.c+(hmap4&15)];
        p[i]=stretch(cr.cm(cr.cxt)>>8);
        break;
      case MATCH: // sizebits bufbits: a=len, b=offset, c=bit, cxt=bitpos,
                  //                   ht=buf, limit=pos
        if (cr.a==0) p[i]=0;
        else {
          cr.c=(cr.ht(cr.limit-cr.b)>>(7-cr.cxt))&1; // predicted bit
          p[i]=stretch(dt2k[cr.a]*(cr.c*-2+1)&32767);
        }
        break;
      case AVG: // j k wt
        p[i]=(p[cp[1]]*cp[3]+p[cp[2]]*(256-cp[3]))>>8;
        break;
      case MIX2: { // sizebits j k rate mask
                   // c=size cm=wt[size] cxt=input
        cr.cxt=((h[i]+(c8&cp[5]))&(cr.c-1));
        int w=cr.a16[cr.cxt];
        p[i]=(w*p[cp[2]]+(65536-w)*p[cp[3]])>>16;
      }
        break;
      case MIX: {  // sizebits j m rate mask
                   // c=size cm=wt[size][m] cxt=index of wt in cm
        int m=cp[3];
        cr.cxt=h[i]+(c8&cp[5]);
        cr.cxt=(cr.cxt&(cr.c-1))*m; // pointer to row of weights
        int* wt=(int*)&cr.cm[cr.cxt];
        p[i]=0;
        for (int j=0; j<m; ++j)
          p[i]+=(wt[j]>>8)*p[cp[2]+j];
        p[i]=clamp2k(p[i]>>8);
      }
        break;
      case ISSE: { // sizebits j -- c=hi, cxt=bh
        if (c8==1 || (c8&0xf0)==16)
          cr.c=find(cr.ht, cp[1]+2, h[i]+16*c8);
        cr.cxt=cr.ht[cr.c+(hmap4&15)];  // bit history
        int *wt=(int*)&cr.cm[cr.cxt*2];
        p[i]=clamp2k((wt[0]*p[cp[2]]+wt[1]*64)>>16);
      }
        break;
      case SSE: { // sizebits j start limit
        cr.cxt=(h[i]+c8)*32;
        int pq=p[cp[2]]+992;
        if (pq<0) pq=0;
        if (pq>1983) pq=1983;
        int wt=pq&63;
        pq>>=6;
        cr.cxt+=pq;
        p[i]=stretch(((cr.cm(cr.cxt)>>10)*(64-wt)+(cr.cm(cr.cxt+1)>>10)*wt)>>13);
        cr.cxt+=wt>>5;
      }
        break;
      default:
        error("component predict not implemented");
    }
    cp+=compsize[cp[0]];
  }
  return squash(p[n-1]);
}

// Update model with decoded bit y (0...1)
void Predictor::update0(int y) {
  // Update components
  const U8* cp=&z.header[7];
  int n=z.header[6];
  for (int i=0; i<n; ++i) {
    Component& cr=comp[i];
    switch(cp[0]) {
      case CONS:  // c
        break;
      case CM:  // sizebits limit
        train(cr, y);
        break;
      case ICM: { // sizebits: cxt=ht[b]=bh, ht[c][0..15]=bh row, cxt=bh
        cr.ht[cr.c+(hmap4&15)]=st.next(cr.ht[cr.c+(hmap4&15)], y);
        U32& pn=cr.cm(cr.cxt);
        pn+=int(y*32767-(pn>>8))>>2;
      }
        break;
      case MATCH: // sizebits bufbits:
                  //   a=len, b=offset, c=bit, cm=index, cxt=bitpos
                  //   ht=buf, limit=pos
      {
        if (int(cr.c)!=y) cr.a=0;  // mismatch?
        cr.ht(cr.limit)+=cr.ht(cr.limit)+y;
        if (++cr.cxt==8) {
          cr.cxt=0;
          ++cr.limit;
          cr.limit&=(1<<cp[2])-1;
          if (cr.a==0) {  // look for a match
            cr.b=cr.limit-cr.cm(h[i]);
            if (cr.b&(cr.ht.size()-1))
              while (cr.a<255
                     && cr.ht(cr.limit-cr.a-1)==cr.ht(cr.limit-cr.a-cr.b-1))
                ++cr.a;
          }
          else cr.a+=cr.a<255;
          cr.cm(h[i])=cr.limit;
        }
      }
        break;
      case AVG:  // j k wt
        break;
      case MIX2: { // sizebits j k rate mask
                   // cm=wt[size], cxt=input
        int err=(y*32767-squash(p[i]))*cp[4]>>5;
        int w=cr.a16[cr.cxt];
        w+=(err*(p[cp[2]]-p[cp[3]])+(1<<12))>>13;
        if (w<0) w=0;
        if (w>65535) w=65535;
        cr.a16[cr.cxt]=w;
      }
        break;
      case MIX: {   // sizebits j m rate mask
                    // cm=wt[size][m], cxt=input
        int m=cp[3];
        int err=(y*32767-squash(p[i]))*cp[4]>>4;
        int* wt=(int*)&cr.cm[cr.cxt];
        for (int j=0; j<m; ++j)
          wt[j]=clamp512k(wt[j]+((err*p[cp[2]+j]+(1<<12))>>13));
      }
        break;
      case ISSE: { // sizebits j  -- c=hi, cxt=bh
        int err=y*32767-squash(p[i]);
        int *wt=(int*)&cr.cm[cr.cxt*2];
        wt[0]=clamp512k(wt[0]+((err*p[cp[2]]+(1<<12))>>13));
        wt[1]=clamp512k(wt[1]+((err+16)>>5));
        cr.ht[cr.c+(hmap4&15)]=st.next(cr.cxt, y);
      }
        break;
      case SSE:  // sizebits j start limit
        train(cr, y);
        break;
      default:
        break;
    }
    cp+=compsize[cp[0]];
  }

  // Save bit y in c8, hmap4
  c8+=c8+y;
  if (c8>=256) {
    z.run(c8-256);
    hmap4=1;
    c8=1;
    for (int i=0; i<n; ++i) h[i]=z.H(i);
  }
  else if (c8>=16 && c8<32)
    hmap4=(hmap4&0xf)<<5|y<<4|1;
  else
    hmap4=(hmap4&0x1f0)|(((hmap4&0xf)*2+y)&0xf);
}

// Find cxt row in hash table ht. ht has rows of 16 indexed by the
// low sizebits of cxt with element 0 having the next higher 8 bits for
// collision detection. If not found after 3 adjacent tries, replace the
// row with lowest element 1 as priority. Return index of row.
size_t Predictor::find(Array<U8>& ht, int sizebits, U32 cxt) {
  int chk=cxt>>sizebits&255;
  size_t h0=(cxt*16)&(ht.size()-16);
  if (ht[h0]==chk) return h0;
  size_t h1=h0^16;
  if (ht[h1]==chk) return h1;
  size_t h2=h0^32;
  if (ht[h2]==chk) return h2;
  if (ht[h0+1]<=ht[h1+1] && ht[h0+1]<=ht[h2+1])
    return memset(&ht[h0], 0, 16), ht[h0]=chk, h0;
  else if (ht[h1+1]<ht[h2+1])
    return memset(&ht[h1], 0, 16), ht[h1]=chk, h1;
  else
    return memset(&ht[h2], 0, 16), ht[h2]=chk, h2;
}

/////////////////////// Decoder ///////////////////////

Decoder::Decoder(ZPAQL& z):
    in(0), low(1), high(0xFFFFFFFF), curr(0), rpos(0), wpos(0),
    pr(z), buf(BUFSIZE) {
}

void Decoder::init() {
  pr.init();
  if (pr.isModeled()) low=1, high=0xFFFFFFFF, curr=0;
  else low=high=curr=0;
}

// Return next bit of decoded input, which has 16 bit probability p of being 1
int Decoder::decode(int p) {
  if (curr<low || curr>high) error("archive corrupted");
  U32 mid=low+U32(((high-low)*U64(U32(p)))>>16);  // split range
  int y;
  if (curr<=mid) y=1, high=mid;  // pick half
  else y=0, low=mid+1;
  while ((high^low)<0x1000000) { // shift out identical leading bytes
    high=high<<8|255;
    low=low<<8;
    low+=(low==0);
    int c=get();
    if (c<0) error("unexpected end of file");
    curr=curr<<8|c;
  }
  return y;
}

// Decompress 1 byte or -1 at end of input
int Decoder::decompress() {
  if (pr.isModeled()) {  // n>0 components?
    if (curr==0) {  // segment initialization
      for (int i=0; i<4; ++i)
        curr=curr<<8|get();
    }
    if (decode(0)) {
      if (curr!=0) error("decoding end of stream");
      return -1;
    }
    else {
      int c=1;
      while (c<256) {  // get 8 bits
        int p=pr.predict()*2+1;
        c+=c+decode(p);
        pr.update(c&1);
      }
      return c-256;
    }
  }
  else {
    if (curr==0) {
      for (int i=0; i<4; ++i) curr=curr<<8|get();
      if (curr==0) return -1;
    }
    --curr;
    return get();
  }
}

// Find end of compressed data and return next byte
int Decoder::skip() {
  int c=-1;
  if (pr.isModeled()) {
    while (curr==0)  // at start?
      curr=get();
    while (curr && (c=get())>=0)  // find 4 zeros
      curr=curr<<8|c;
    while ((c=get())==0) ;  // might be more than 4
    return c;
  }
  else {
    if (curr==0)  // at start?
      for (int i=0; i<4 && (c=get())>=0; ++i) curr=curr<<8|c;
    while (curr>0) {
      while (curr>0) {
        --curr;
        if (get()<0) return error("skipped to EOF"), -1;
      }
      for (int i=0; i<4 && (c=get())>=0; ++i) curr=curr<<8|c;
    }
    if (c>=0) c=get();
    return c;
  }
}

////////////////////// PostProcessor //////////////////////

// Copy ph, pm from block header
void PostProcessor::init(int h, int m) {
  state=hsize=0;
  ph=h;
  pm=m;
  z.clear();
}

// (PASS=0 | PROG=1 psize[0..1] pcomp[0..psize-1]) data... EOB=-1
// Return state: 1=PASS, 2..4=loading PROG, 5=PROG loaded
int PostProcessor::write(int c) {
  switch (state) {
    case 0:  // initial state
      if (c<0) error("Unexpected EOS");
      state=c+1;  // 1=PASS, 2=PROG
      if (state>2) error("unknown post processing type");
      if (state==1) z.clear();
      break;
    case 1:  // PASS
      z.outc(c);
      break;
    case 2: // PROG
      if (c<0) error("Unexpected EOS");
      hsize=c;  // low byte of size
      state=3;
      break;
    case 3:  // PROG psize[0]
      if (c<0) error("Unexpected EOS");
      hsize+=c*256;  // high byte of psize
      if (hsize<1) error("Empty PCOMP");
      z.header.resize(hsize+300);
      z.cend=8;
      z.hbegin=z.hend=z.cend+128;
      z.header[4]=ph;
      z.header[5]=pm;
      state=4;
      break;
    case 4:  // PROG psize[0..1] pcomp[0...]
      if (c<0) error("Unexpected EOS");
      z.header[z.hend++]=c;  // one byte of pcomp
      if (z.hend-z.hbegin==hsize) {  // last byte of pcomp?
        hsize=z.cend-2+z.hend-z.hbegin;
        z.header[0]=hsize&255;  // header size with empty COMP
        z.header[1]=hsize>>8;
        z.initp();
        state=5;
      }
      break;
    case 5:  // PROG ... data
      z.run(c);
      if (c<0) z.flush();
      break;
  }
  return state;
}

/////////////////////// Decompresser /////////////////////

// Find the start of a block and return true if found. Set memptr
// to memory used.
bool Decompresser::findBlock(double* memptr) {
  // Find start of block
  U32 h1=0x3D49B113, h2=0x29EB7F93, h3=0x2614BE13, h4=0x3828EB13;
  // Rolling hashes initialized to hash of first 13 bytes
  int c;
  while ((c=dec.get())!=-1) {
    h1=h1*12+c;
    h2=h2*20+c;
    h3=h3*28+c;
    h4=h4*44+c;
    if (h1==0xB16B88F1 && h2==0xFF5376F1 && h3==0x72AC5BF1 && h4==0x2F909AF1)
      break;  // hash of 16 byte string
  }
  if (c==-1) return false;

  // Read header
  if ((c=dec.get())!=1 && c!=2) error("unsupported ZPAQ level");
  if (dec.get()!=1) error("unsupported ZPAQL type");
  z.read(&dec);
  if (c==1 && z.header.isize()>6 && z.header[6]==0)
    error("ZPAQ level 1 requires at least 1 component");
  if (memptr) *memptr=z.memory();
  state=FILENAME;
  decode_state=FIRSTSEG;
  return true;
}

// Read the start of a segment (1) or end of block code (255).
// If a segment is found, write the filename and return true, else false.
bool Decompresser::findFilename(Writer* filename) {
  int c=dec.get();
  if (c==1) {  // segment found
    while (true) {
      c=dec.get();
      if (c==-1) error("unexpected EOF");
      if (c==0) {
        state=COMMENT;
        return true;
      }
      if (filename) filename->put(c);
    }
  }
  else if (c==255) {  // end of block found
    state=BLOCK;
    return false;
  }
  else
    error("missing segment or end of block");
  return false;
}

// Read the comment from the segment header
void Decompresser::readComment(Writer* comment) {
  state=DATA;
  while (true) {
    int c=dec.get();
    if (c==-1) error("unexpected EOF");
    if (c==0) break;
    if (comment) comment->put(c);
  }
  if (dec.get()!=0) error("missing reserved byte");
}

// Decompress n bytes, or all if n < 0. Return false if done
bool Decompresser::decompress(int n) {
  if (decode_state==SKIP) error("decompression after skipped segment");

  // Initialize models to start decompressing block
  if (decode_state==FIRSTSEG) {
    dec.init();
    pp.init(z.header[4], z.header[5]);
    decode_state=SEG;
  }

  // Decompress and load PCOMP into postprocessor
  while ((pp.getState()&3)!=1)
    pp.write(dec.decompress());

  // Decompress n bytes, or all if n < 0
  while (n) {
    int c=dec.decompress();
    pp.write(c);
    if (c==-1) {
      state=SEGEND;
      return false;
    }
    if (n>0) --n;
  }
  return true;
}

// Read end of block. If a SHA1 checksum is present, write 1 and the
// 20 byte checksum into sha1string, else write 0 in first byte.
// If sha1string is 0 then discard it.
void Decompresser::readSegmentEnd(char* sha1string) {
  // Skip remaining data if any and get next byte
  int c=0;
  if (state==DATA) {
    c=dec.skip();
    decode_state=SKIP;
  }
  else if (state==SEGEND)
    c=dec.get();
  state=FILENAME;

  // Read checksum
  if (c==254) {
    if (sha1string) sha1string[0]=0;  // no checksum
  }
  else if (c==253) {
    if (sha1string) sha1string[0]=1;
    for (int i=1; i<=20; ++i) {
      c=dec.get();
      if (sha1string) sha1string[i]=c;
    }
  }
  else
    error("missing end of segment marker");
}

/////////////////////////// decompress() //////////////////////

void decompress(Reader* in, Writer* out) {
  Decompresser d;
  d.setInput(in);
  d.setOutput(out);
  while (d.findBlock()) {       // don't calculate memory
    while (d.findFilename()) {  // discard filename
      d.readComment();          // discard comment
      d.decompress();           // to end of segment
      d.readSegmentEnd();       // discard sha1string
    }
  }
}

/////////////////////////// Encoder ///////////////////////////

// Initialize for start of block
void Encoder::init() {
  low=1;
  high=0xFFFFFFFF;
  pr.init();
  if (!pr.isModeled()) low=0, buf.resize(1<<16);
}

// compress bit y having probability p/64K
void Encoder::encode(int y, int p) {
  U32 mid=low+U32(((high-low)*U64(U32(p)))>>16);  // split range
  if (y) high=mid; else low=mid+1; // pick half
  while ((high^low)<0x1000000) { // write identical leading bytes
    out->put(high>>24);  // same as low>>24
    high=high<<8|255;
    low=low<<8;
    low+=(low==0); // so we don't code 4 0 bytes in a row
  }
}

// compress byte c (0..255 or -1=EOS)
void Encoder::compress(int c) {
  if (pr.isModeled()) {
    if (c==-1)
      encode(1, 0);
    else {
      encode(0, 0);
      for (int i=7; i>=0; --i) {
        int p=pr.predict()*2+1;
        int y=c>>i&1;
        encode(y, p);
        pr.update(y);
      }
    }
  }
  else {
    if (low && (c<0 || low==buf.size())) {
      out->put((low>>24)&255);
      out->put((low>>16)&255);
      out->put((low>>8)&255);
      out->put(low&255);
      out->write(&buf[0], low);
      low=0;
    }
    if (c>=0) buf[low++]=c;
  }
}

//////////////////////////// Compiler /////////////////////////

// Component names
const char* compname[256]=
  {"","const","cm","icm","match","avg","mix2","mix","isse","sse",0};

// Opcodes
const char* opcodelist[272]={
"error","a++",  "a--",  "a!",   "a=0",  "",     "",     "a=r",
"b<>a", "b++",  "b--",  "b!",   "b=0",  "",     "",     "b=r",
"c<>a", "c++",  "c--",  "c!",   "c=0",  "",     "",     "c=r",
"d<>a", "d++",  "d--",  "d!",   "d=0",  "",     "",     "d=r",
"*b<>a","*b++", "*b--", "*b!",  "*b=0", "",     "",     "jt",
"*c<>a","*c++", "*c--", "*c!",  "*c=0", "",     "",     "jf",
"*d<>a","*d++", "*d--", "*d!",  "*d=0", "",     "",     "r=a",
"halt", "out",  "",     "hash", "hashd","",     "",     "jmp",
"a=a",  "a=b",  "a=c",  "a=d",  "a=*b", "a=*c", "a=*d", "a=",
"b=a",  "b=b",  "b=c",  "b=d",  "b=*b", "b=*c", "b=*d", "b=",
"c=a",  "c=b",  "c=c",  "c=d",  "c=*b", "c=*c", "c=*d", "c=",
"d=a",  "d=b",  "d=c",  "d=d",  "d=*b", "d=*c", "d=*d", "d=",
"*b=a", "*b=b", "*b=c", "*b=d", "*b=*b","*b=*c","*b=*d","*b=",
"*c=a", "*c=b", "*c=c", "*c=d", "*c=*b","*c=*c","*c=*d","*c=",
"*d=a", "*d=b", "*d=c", "*d=d", "*d=*b","*d=*c","*d=*d","*d=",
"",     "",     "",     "",     "",     "",     "",     "",
"a+=a", "a+=b", "a+=c", "a+=d", "a+=*b","a+=*c","a+=*d","a+=",
"a-=a", "a-=b", "a-=c", "a-=d", "a-=*b","a-=*c","a-=*d","a-=",
"a*=a", "a*=b", "a*=c", "a*=d", "a*=*b","a*=*c","a*=*d","a*=",
"a/=a", "a/=b", "a/=c", "a/=d", "a/=*b","a/=*c","a/=*d","a/=",
"a%=a", "a%=b", "a%=c", "a%=d", "a%=*b","a%=*c","a%=*d","a%=",
"a&=a", "a&=b", "a&=c", "a&=d", "a&=*b","a&=*c","a&=*d","a&=",
"a&~a", "a&~b", "a&~c", "a&~d", "a&~*b","a&~*c","a&~*d","a&~",
"a|=a", "a|=b", "a|=c", "a|=d", "a|=*b","a|=*c","a|=*d","a|=",
"a^=a", "a^=b", "a^=c", "a^=d", "a^=*b","a^=*c","a^=*d","a^=",
"a<<=a","a<<=b","a<<=c","a<<=d","a<<=*b","a<<=*c","a<<=*d","a<<=",
"a>>=a","a>>=b","a>>=c","a>>=d","a>>=*b","a>>=*c","a>>=*d","a>>=",
"a==a", "a==b", "a==c", "a==d", "a==*b","a==*c","a==*d","a==",
"a<a",  "a<b",  "a<c",  "a<d",  "a<*b", "a<*c", "a<*d", "a<",
"a>a",  "a>b",  "a>c",  "a>d",  "a>*b", "a>*c", "a>*d", "a>",
"",     "",     "",     "",     "",     "",     "",     "",
"",     "",     "",     "",     "",     "",     "",     "lj",
"post", "pcomp","end",  "if",   "ifnot","else", "endif","do",
"while","until","forever","ifl","ifnotl","elsel",";",    0};

// Advance in to start of next token. Tokens are delimited by white
// space. Comments inclosed in ((nested) parenthsis) are skipped.
void Compiler::next() {
  for (; *in; ++in) {
    if (*in=='\n') ++line;
    if (*in=='(') state+=1+(state<0);
    else if (state>0 && *in==')') --state;
    else if (state<0 && *in<=' ') state=0;
    else if (state==0 && *in>' ') {state=-1; break;}
  }
  if (!*in) error("unexpected end of config");
}

// convert to lower case
int tolower(int c) {return (c>='A' && c<='Z') ? c+'a'-'A' : c;}

// return true if in==word up to white space or '(', case insensitive
bool Compiler::matchToken(const char* word) {
  const char* a=in;
  for (; (*a>' ' && *a!='(' && *word); ++a, ++word)
    if (tolower(*a)!=tolower(*word)) return false;
  return !*word && (*a<=' ' || *a=='(');
}

// Print error message and exit
void Compiler::syntaxError(const char* msg, const char* expected) {
  Array<char> sbuf(128);  // error message to report
  char* s=&sbuf[0];
  strcat(s, "Config line ");
  for (int i=strlen(s), r=1000000; r; r/=10)  // append line number
    if (line/r) s[i++]='0'+line/r%10;
  strcat(s, " at ");
  for (int i=strlen(s); i<40 && *in>' '; ++i)  // append token found
    s[i]=*in++;
  strcat(s, ": ");
  strncat(s, msg, 40);  // append message
  if (expected) {
    strcat(s, ", expected: ");
    strncat(s, expected, 20);  // append expected token if any
  }
  error(s);
}

// Read a token, which must be in the NULL terminated list or else
// exit with an error. If found, return its index.
int Compiler::rtoken(const char* list[]) {
  next();
  for (int i=0; list[i]; ++i)
    if (matchToken(list[i]))
      return i;
  syntaxError("unexpected");
  return -1; // not reached
}

// Read a token which must be the specified value s
void Compiler::rtoken(const char* s) {
  next();
  if (!matchToken(s)) syntaxError("expected", s);
}

// Read a number in (low...high) or exit with an error
// For numbers like $N+M, return arg[N-1]+M
int Compiler::rtoken(int low, int high) {
  next();
  int r=0;
  if (in[0]=='$' && in[1]>='1' && in[1]<='9') {
    if (in[2]=='+') r=atoi(in+3);
    if (args) r+=args[in[1]-'1'];
  }
  else if (in[0]=='-' || (in[0]>='0' && in[0]<='9')) r=atoi(in);
  else syntaxError("expected a number");
  if (r<low) syntaxError("number too low");
  if (r>high) syntaxError("number too high");
  return r;
}

// Compile HCOMP or PCOMP code. Exit on error. Return
// code for end token (POST, PCOMP, END)
int Compiler::compile_comp(ZPAQL& z) {
  int op=0;
  const int comp_begin=z.hend;
  while (true) {
    op=rtoken(opcodelist);
    if (op==POST || op==PCOMP || op==END) break;
    int operand=-1; // 0...255 if 2 bytes
    int operand2=-1;  // 0...255 if 3 bytes
    if (op==IF) {
      op=JF;
      operand=0; // set later
      if_stack.push(z.hend+1); // save jump target location
    }
    else if (op==IFNOT) {
      op=JT;
      operand=0;
      if_stack.push(z.hend+1); // save jump target location
    }
    else if (op==IFL || op==IFNOTL) {  // long if
      if (op==IFL) z.header[z.hend++]=(JT);
      if (op==IFNOTL) z.header[z.hend++]=(JF);
      z.header[z.hend++]=(3);
      op=LJ;
      operand=operand2=0;
      if_stack.push(z.hend+1);
    }
    else if (op==ELSE || op==ELSEL) {
      if (op==ELSE) op=JMP, operand=0;
      if (op==ELSEL) op=LJ, operand=operand2=0;
      int a=if_stack.pop();  // conditional jump target location
      if (z.header[a-1]!=LJ) {  // IF, IFNOT
        int j=z.hend-a+1+(op==LJ); // offset at IF
        if (j>127) syntaxError("IF too big, try IFL, IFNOTL");
        z.header[a]=j;
      }
      else {  // IFL, IFNOTL
        int j=z.hend-comp_begin+2+(op==LJ);
        z.header[a]=j&255;
        z.header[a+1]=(j>>8)&255;
      }
      if_stack.push(z.hend+1);  // save JMP target location
    }
    else if (op==ENDIF) {
      int a=if_stack.pop();  // jump target address
      int j=z.hend-a-1;  // jump offset
      if (z.header[a-1]!=LJ) {
        if (j>127) syntaxError("IF too big, try IFL, IFNOTL, ELSEL\n");
        z.header[a]=j;
      }
      else {
        j=z.hend-comp_begin;
        z.header[a]=j&255;
        z.header[a+1]=(j>>8)&255;
      }
    }
    else if (op==DO) {
      do_stack.push(z.hend);
    }
    else if (op==WHILE || op==UNTIL || op==FOREVER) {
      int a=do_stack.pop();
      int j=a-z.hend-2;
      if (j>=-127) {  // backward short jump
        if (op==WHILE) op=JT;
        if (op==UNTIL) op=JF;
        if (op==FOREVER) op=JMP;
        operand=j&255;
      }
      else {  // backward long jump
        j=a-comp_begin;
        if (op==WHILE) {
          z.header[z.hend++]=(JF);
          z.header[z.hend++]=(3);
        }
        if (op==UNTIL) {
          z.header[z.hend++]=(JT);
          z.header[z.hend++]=(3);
        }
        op=LJ;
        operand=j&255;
        operand2=j>>8;
      }
    }
    else if ((op&7)==7) { // 2 byte operand, read N
      if (op==LJ) {
        operand=rtoken(0, 65535);
        operand2=operand>>8;
        operand&=255;
      }
      else if (op==JT || op==JF || op==JMP) {
        operand=rtoken(-128, 127);
        operand&=255;
      }
      else
        operand=rtoken(0, 255);
    }
    if (op>=0 && op<=255)
      z.header[z.hend++]=(op);
    if (operand>=0)
      z.header[z.hend++]=(operand);
    if (operand2>=0)
      z.header[z.hend++]=(operand2);
    if (z.hend>=z.header.isize()-130 || z.hend-z.hbegin+z.cend-2>65535)
      syntaxError("program too big");
  }
  z.header[z.hend++]=(0); // END
  return op;
}

// Compile a configuration file. Store COMP/HCOMP section in hcomp.
// If there is a PCOMP section, store it in pcomp and store the PCOMP
// command in pcomp_cmd. Replace "$1..$9+n" with args[0..8]+n

Compiler::Compiler(const char* in_, int* args_, ZPAQL& hz_, ZPAQL& pz_,
                   Writer* out2_): in(in_), args(args_), hz(hz_), pz(pz_),
                   out2(out2_), if_stack(1000), do_stack(1000) {
  line=1;
  state=0;
  hz.clear();
  pz.clear();
  hz.header.resize(68000); 

  // Compile the COMP section of header
  rtoken("comp");
  hz.header[2]=rtoken(0, 255);  // hh
  hz.header[3]=rtoken(0, 255);  // hm
  hz.header[4]=rtoken(0, 255);  // ph
  hz.header[5]=rtoken(0, 255);  // pm
  const int n=hz.header[6]=rtoken(0, 255);  // n
  hz.cend=7;
  for (int i=0; i<n; ++i) {
    rtoken(i, i);
    CompType type=CompType(rtoken(compname));
    hz.header[hz.cend++]=type;
    int clen=libzpaq::compsize[type&255];
    if (clen<1 || clen>10) syntaxError("invalid component");
    for (int j=1; j<clen; ++j)
      hz.header[hz.cend++]=rtoken(0, 255);  // component arguments
  }
  hz.header[hz.cend++];  // end
  hz.hbegin=hz.hend=hz.cend+128;

  // Compile HCOMP
  rtoken("hcomp");
  int op=compile_comp(hz);

  // Compute header size
  int hsize=hz.cend-2+hz.hend-hz.hbegin;
  hz.header[0]=hsize&255;
  hz.header[1]=hsize>>8;

  // Compile POST 0 END
  if (op==POST) {
    rtoken(0, 0);
    rtoken("end");
  }

  // Compile PCOMP pcomp_cmd ; program... END
  else if (op==PCOMP) {
    pz.header.resize(68000);
    pz.header[4]=hz.header[4];  // ph
    pz.header[5]=hz.header[5];  // pm
    pz.cend=8;
    pz.hbegin=pz.hend=pz.cend+128;

    // get pcomp_cmd ending with ";" (case sensitive)
    next();
    while (*in && *in!=';') {
      if (out2)
        out2->put(*in);
      ++in;
    }
    if (*in) ++in;

    // Compile PCOMP
    op=compile_comp(pz);
    int len=pz.cend-2+pz.hend-pz.hbegin;  // insert header size
    pz.header[0]=len&255;
    pz.header[1]=len>>8;
    if (op!=END)
      syntaxError("expected END");
  }
  else if (op!=END)
    syntaxError("expected END or POST 0 END or PCOMP cmd ; ... END");
}

///////////////////// Compressor //////////////////////

// Write 13 byte start tag
// "\x37\x6B\x53\x74\xA0\x31\x83\xD3\x8C\xB2\x28\xB0\xD3"
void Compressor::writeTag() {
  enc.out->put(0x37);
  enc.out->put(0x6b);
  enc.out->put(0x53);
  enc.out->put(0x74);
  enc.out->put(0xa0);
  enc.out->put(0x31);
  enc.out->put(0x83);
  enc.out->put(0xd3);
  enc.out->put(0x8c);
  enc.out->put(0xb2);
  enc.out->put(0x28);
  enc.out->put(0xb0);
  enc.out->put(0xd3);
}

void Compressor::startBlock(int level) {

  // Model 1 - min.cfg
  static const char models[]={
  26,0,1,2,0,0,2,3,16,8,19,0,0,96,4,28,
  59,10,59,112,25,10,59,10,59,112,56,0,

  // Model 2 - mid.cfg
  69,0,3,3,0,0,8,3,5,8,13,0,8,17,1,8,
  18,2,8,18,3,8,19,4,4,22,24,7,16,0,7,24,
  (char)-1,0,17,104,74,4,95,1,59,112,10,25,59,112,10,25,
  59,112,10,25,59,112,10,25,59,112,10,25,59,10,59,112,
  25,69,(char)-49,8,112,56,0,

  // Model 3 - max.cfg
  (char)-60,0,5,9,0,0,22,1,(char)-96,3,5,8,13,1,8,16,
  2,8,18,3,8,19,4,8,19,5,8,20,6,4,22,24,
  3,17,8,19,9,3,13,3,13,3,13,3,14,7,16,0,
  15,24,(char)-1,7,8,0,16,10,(char)-1,6,0,15,16,24,0,9,
  8,17,32,(char)-1,6,8,17,18,16,(char)-1,9,16,19,32,(char)-1,6,
  0,19,20,16,0,0,17,104,74,4,95,2,59,112,10,25,
  59,112,10,25,59,112,10,25,59,112,10,25,59,112,10,25,
  59,10,59,112,10,25,59,112,10,25,69,(char)-73,32,(char)-17,64,47,
  14,(char)-25,91,47,10,25,60,26,48,(char)-122,(char)-105,20,112,63,9,70,
  (char)-33,0,39,3,25,112,26,52,25,25,74,10,4,59,112,25,
  10,4,59,112,25,10,4,59,112,25,65,(char)-113,(char)-44,72,4,59,
  112,8,(char)-113,(char)-40,8,68,(char)-81,60,60,25,69,(char)-49,9,112,25,25,
  25,25,25,112,56,0,

  0,0}; // 0,0 = end of list

  if (level<1) error("compression level must be at least 1");
  const char* p=models;
  int i;
  for (i=1; i<level && toU16(p); ++i)
    p+=toU16(p)+2;
  if (toU16(p)<1) error("compression level too high");
  startBlock(p);
}

// Memory reader
class MemoryReader: public Reader {
  const char* p;
public:
  MemoryReader(const char* p_): p(p_) {}
  int get() {return *p++&255;}
};

void Compressor::startBlock(const char* hcomp) {
  MemoryReader m(hcomp);
  z.read(&m);
  pz.sha1=&sha1;
  enc.out->put('z');
  enc.out->put('P');
  enc.out->put('Q');
  enc.out->put(1+(z.header[6]==0));  // level 1 or 2
  enc.out->put(1);
  z.write(enc.out, false);
  state=BLOCK1;
}

void Compressor::startBlock(const char* config, int* args, Writer* pcomp_cmd) {
  Compiler(config, args, z, pz, pcomp_cmd);
  pz.sha1=&sha1;
  enc.out->put('z');
  enc.out->put('P');
  enc.out->put('Q');
  enc.out->put(1+(z.header[6]==0));  // level 1 or 2
  enc.out->put(1);
  z.write(enc.out, false);
  state=BLOCK1;
}

// Write a segment header
void Compressor::startSegment(const char* filename, const char* comment) {
  enc.out->put(1);
  while (filename && *filename)
    enc.out->put(*filename++);
  enc.out->put(0);
  while (comment && *comment)
    enc.out->put(*comment++);
  enc.out->put(0);
  enc.out->put(0);
  if (state==BLOCK1) state=SEG1;
  if (state==BLOCK2) state=SEG2;
}

// Initialize encoding and write pcomp to first segment
// If len is 0 then length is encoded in pcomp[0..1]
// if pcomp is 0 then get pcomp from pz.header
void Compressor::postProcess(const char* pcomp, int len) {
  if (state==SEG2) return;
  enc.init();
  if (!pcomp) {
    len=pz.hend-pz.hbegin;
    if (len>0) {
      pcomp=(const char*)&pz.header[pz.hbegin];
    }
  }
  else if (len==0) {
    len=toU16(pcomp);
    pcomp+=2;
  }
  if (len>0) {
    enc.compress(1);
    enc.compress(len&255);
    enc.compress((len>>8)&255);
    for (int i=0; i<len; ++i)
      enc.compress(pcomp[i]&255);
    if (verify)
      pz.initp();
  }
  else
    enc.compress(0);
  state=SEG2;
}

// Compress n bytes, or to EOF if n < 0
bool Compressor::compress(int n) {
  if (state==SEG1)
    postProcess();

  const int BUFSIZE=1<<14;
  char buf[BUFSIZE];  // input buffer
  while (n) {
    int nbuf=BUFSIZE;  // bytes read into buf
    if (n>=0 && n<nbuf) nbuf=n;
    int nr=in->read(buf, nbuf);
    if (nr<0 || nr>BUFSIZE || nr>nbuf) error("invalid read size");
    if (nr<=0) return false;
    if (n>=0) n-=nr;
    for (int i=0; i<nr; ++i) {
      int ch=U8(buf[i]);
      enc.compress(ch);
      if (verify) {
        if (pz.hend) pz.run(ch);
        else sha1.put(ch);
      }
    }
  }
  return true;
}

// End segment, write sha1string if present
void Compressor::endSegment(const char* sha1string) {
  if (state==SEG1)
    postProcess();
  enc.compress(-1);
  if (verify && pz.hend) {
    pz.run(-1);
    pz.flush();
  }
  enc.out->put(0);
  enc.out->put(0);
  enc.out->put(0);
  enc.out->put(0);
  if (sha1string) {
    enc.out->put(253);
    for (int i=0; i<20; ++i)
      enc.out->put(sha1string[i]);
  }
  else
    enc.out->put(254);
  state=BLOCK2;
}

// End segment, write checksum and size is verify is true
char* Compressor::endSegmentChecksum(int64_t* size, bool dosha1) {
  if (state==SEG1)
    postProcess();
  enc.compress(-1);
  if (verify && pz.hend) {
    pz.run(-1);
    pz.flush();
  }
  enc.out->put(0);
  enc.out->put(0);
  enc.out->put(0);
  enc.out->put(0);
  if (verify) {
    if (size) *size=sha1.usize();
    memcpy(sha1result, sha1.result(), 20);
  }
  if (verify && dosha1) {
    enc.out->put(253);
    for (int i=0; i<20; ++i)
      enc.out->put(sha1result[i]);
  }
  else
    enc.out->put(254);
  state=BLOCK2;
  return verify ? sha1result : 0;
}

// End block
void Compressor::endBlock() {
  enc.out->put(255);
  state=INIT;
}

/////////////////////////// compress() ///////////////////////

void compress(Reader* in, Writer* out, const char* method,
              const char* filename, const char* comment, bool dosha1) {

  // Get block size
  int bs=4;
  if (method && method[0] && method[1]>='0' && method[1]<='9') {
    bs=method[1]-'0';
    if (method[2]>='0' && method[2]<='9') bs=bs*10+method[2]-'0';
    if (bs>11) bs=11;
  }
  bs=(0x100000<<bs)-4096;

  // Compress in blocks
  StringBuffer sb(bs);
  sb.write(0, bs);
  int n=0;
  while (in && (n=in->read((char*)sb.data(), bs))>0) {
    sb.resize(n);
    compressBlock(&sb, out, method, filename, comment, dosha1);
    filename=0;
    comment=0;
    sb.resize(0);
  }
}

//////////////////////// ZPAQL::assemble() ////////////////////

// Return a prediction of the next bit in range 0..32767
// Use JIT code starting at pcode[0] if available, or else create it.
int Predictor::predict() {
  return predict0();
}

// Update the model with bit y = 0..1
// Use the JIT code starting at pcode[5].
void Predictor::update(int y) {
  update0(y);
}

// Execute the ZPAQL code with input byte or -1 for EOF.
// Use JIT code at rcode if available, or else create it.
void ZPAQL::run(U32 input) {
  run0(input);
}

////////////////////////// divsufsort ///////////////////////////////

/*
 * divsufsort.c for libdivsufsort-lite
 * Copyright (c) 2003-2008 Yuta Mori All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/*- Constants -*/
#define INLINE __inline
#if defined(ALPHABET_SIZE) && (ALPHABET_SIZE < 1)
# undef ALPHABET_SIZE
#endif
#if !defined(ALPHABET_SIZE)
# define ALPHABET_SIZE (256)
#endif
#define BUCKET_A_SIZE (ALPHABET_SIZE)
#define BUCKET_B_SIZE (ALPHABET_SIZE * ALPHABET_SIZE)
#if defined(SS_INSERTIONSORT_THRESHOLD)
# if SS_INSERTIONSORT_THRESHOLD < 1
#  undef SS_INSERTIONSORT_THRESHOLD
#  define SS_INSERTIONSORT_THRESHOLD (1)
# endif
#else
# define SS_INSERTIONSORT_THRESHOLD (8)
#endif
#if defined(SS_BLOCKSIZE)
# if SS_BLOCKSIZE < 0
#  undef SS_BLOCKSIZE
#  define SS_BLOCKSIZE (0)
# elif 32768 <= SS_BLOCKSIZE
#  undef SS_BLOCKSIZE
#  define SS_BLOCKSIZE (32767)
# endif
#else
# define SS_BLOCKSIZE (1024)
#endif
/* minstacksize = log(SS_BLOCKSIZE) / log(3) * 2 */
#if SS_BLOCKSIZE == 0
# define SS_MISORT_STACKSIZE (96)
#elif SS_BLOCKSIZE <= 4096
# define SS_MISORT_STACKSIZE (16)
#else
# define SS_MISORT_STACKSIZE (24)
#endif
#define SS_SMERGE_STACKSIZE (32)
#define TR_INSERTIONSORT_THRESHOLD (8)
#define TR_STACKSIZE (64)


/*- Macros -*/
#ifndef SWAP
# define SWAP(_a, _b) do { t = (_a); (_a) = (_b); (_b) = t; } while(0)
#endif /* SWAP */
#ifndef MIN
# define MIN(_a, _b) (((_a) < (_b)) ? (_a) : (_b))
#endif /* MIN */
#ifndef MAX
# define MAX(_a, _b) (((_a) > (_b)) ? (_a) : (_b))
#endif /* MAX */
#define STACK_PUSH(_a, _b, _c, _d)\
  do {\
    stack[ssize].a = (_a), stack[ssize].b = (_b),\
    stack[ssize].c = (_c), stack[ssize++].d = (_d);\
  } while(0)
#define STACK_PUSH5(_a, _b, _c, _d, _e)\
  do {\
    stack[ssize].a = (_a), stack[ssize].b = (_b),\
    stack[ssize].c = (_c), stack[ssize].d = (_d), stack[ssize++].e = (_e);\
  } while(0)
#define STACK_POP(_a, _b, _c, _d)\
  do {\
    if(ssize == 0) { return; }\
    (_a) = stack[--ssize].a, (_b) = stack[ssize].b,\
    (_c) = stack[ssize].c, (_d) = stack[ssize].d;\
  } while(0)
#define STACK_POP5(_a, _b, _c, _d, _e)\
  do {\
    if(ssize == 0) { return; }\
    (_a) = stack[--ssize].a, (_b) = stack[ssize].b,\
    (_c) = stack[ssize].c, (_d) = stack[ssize].d, (_e) = stack[ssize].e;\
  } while(0)
#define BUCKET_A(_c0) bucket_A[(_c0)]
#if ALPHABET_SIZE == 256
#define BUCKET_B(_c0, _c1) (bucket_B[((_c1) << 8) | (_c0)])
#define BUCKET_BSTAR(_c0, _c1) (bucket_B[((_c0) << 8) | (_c1)])
#else
#define BUCKET_B(_c0, _c1) (bucket_B[(_c1) * ALPHABET_SIZE + (_c0)])
#define BUCKET_BSTAR(_c0, _c1) (bucket_B[(_c0) * ALPHABET_SIZE + (_c1)])
#endif


/*- Private Functions -*/

static const int lg_table[256]= {
 -1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
  5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
};

#if (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE)

static INLINE
int
ss_ilg(int n) {
#if SS_BLOCKSIZE == 0
  return (n & 0xffff0000) ?
          ((n & 0xff000000) ?
            24 + lg_table[(n >> 24) & 0xff] :
            16 + lg_table[(n >> 16) & 0xff]) :
          ((n & 0x0000ff00) ?
             8 + lg_table[(n >>  8) & 0xff] :
             0 + lg_table[(n >>  0) & 0xff]);
#elif SS_BLOCKSIZE < 256
  return lg_table[n];
#else
  return (n & 0xff00) ?
          8 + lg_table[(n >> 8) & 0xff] :
          0 + lg_table[(n >> 0) & 0xff];
#endif
}

#endif /* (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE) */

#if SS_BLOCKSIZE != 0

static const int sqq_table[256] = {
  0,  16,  22,  27,  32,  35,  39,  42,  45,  48,  50,  53,  55,  57,  59,  61,
 64,  65,  67,  69,  71,  73,  75,  76,  78,  80,  81,  83,  84,  86,  87,  89,
 90,  91,  93,  94,  96,  97,  98,  99, 101, 102, 103, 104, 106, 107, 108, 109,
110, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
128, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,
143, 144, 144, 145, 146, 147, 148, 149, 150, 150, 151, 152, 153, 154, 155, 155,
156, 157, 158, 159, 160, 160, 161, 162, 163, 163, 164, 165, 166, 167, 167, 168,
169, 170, 170, 171, 172, 173, 173, 174, 175, 176, 176, 177, 178, 178, 179, 180,
181, 181, 182, 183, 183, 184, 185, 185, 186, 187, 187, 188, 189, 189, 190, 191,
192, 192, 193, 193, 194, 195, 195, 196, 197, 197, 198, 199, 199, 200, 201, 201,
202, 203, 203, 204, 204, 205, 206, 206, 207, 208, 208, 209, 209, 210, 211, 211,
212, 212, 213, 214, 214, 215, 215, 216, 217, 217, 218, 218, 219, 219, 220, 221,
221, 222, 222, 223, 224, 224, 225, 225, 226, 226, 227, 227, 228, 229, 229, 230,
230, 231, 231, 232, 232, 233, 234, 234, 235, 235, 236, 236, 237, 237, 238, 238,
239, 240, 240, 241, 241, 242, 242, 243, 243, 244, 244, 245, 245, 246, 246, 247,
247, 248, 248, 249, 249, 250, 250, 251, 251, 252, 252, 253, 253, 254, 254, 255
};

static INLINE
int
ss_isqrt(int x) {
  int y, e;

  if(x >= (SS_BLOCKSIZE * SS_BLOCKSIZE)) { return SS_BLOCKSIZE; }
  e = (x & 0xffff0000) ?
        ((x & 0xff000000) ?
          24 + lg_table[(x >> 24) & 0xff] :
          16 + lg_table[(x >> 16) & 0xff]) :
        ((x & 0x0000ff00) ?
           8 + lg_table[(x >>  8) & 0xff] :
           0 + lg_table[(x >>  0) & 0xff]);

  if(e >= 16) {
    y = sqq_table[x >> ((e - 6) - (e & 1))] << ((e >> 1) - 7);
    if(e >= 24) { y = (y + 1 + x / y) >> 1; }
    y = (y + 1 + x / y) >> 1;
  } else if(e >= 8) {
    y = (sqq_table[x >> ((e - 6) - (e & 1))] >> (7 - (e >> 1))) + 1;
  } else {
    return sqq_table[x] >> 4;
  }

  return (x < (y * y)) ? y - 1 : y;
}

#endif /* SS_BLOCKSIZE != 0 */


/*---------------------------------------------------------------------------*/

/* Compares two suffixes. */
static INLINE
int
ss_compare(const unsigned char *T,
           const int *p1, const int *p2,
           int depth) {
  const unsigned char *U1, *U2, *U1n, *U2n;

  for(U1 = T + depth + *p1,
      U2 = T + depth + *p2,
      U1n = T + *(p1 + 1) + 2,
      U2n = T + *(p2 + 1) + 2;
      (U1 < U1n) && (U2 < U2n) && (*U1 == *U2);
      ++U1, ++U2) {
  }

  return U1 < U1n ?
        (U2 < U2n ? *U1 - *U2 : 1) :
        (U2 < U2n ? -1 : 0);
}


/*---------------------------------------------------------------------------*/

#if (SS_BLOCKSIZE != 1) && (SS_INSERTIONSORT_THRESHOLD != 1)

/* Insertionsort for small size groups */
static
void
ss_insertionsort(const unsigned char *T, const int *PA,
                 int *first, int *last, int depth) {
  int *i, *j;
  int t;
  int r;

  for(i = last - 2; first <= i; --i) {
    for(t = *i, j = i + 1; 0 < (r = ss_compare(T, PA + t, PA + *j, depth));) {
      do { *(j - 1) = *j; } while((++j < last) && (*j < 0));
      if(last <= j) { break; }
    }
    if(r == 0) { *j = ~*j; }
    *(j - 1) = t;
  }
}

#endif /* (SS_BLOCKSIZE != 1) && (SS_INSERTIONSORT_THRESHOLD != 1) */


/*---------------------------------------------------------------------------*/

#if (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE)

static INLINE
void
ss_fixdown(const unsigned char *Td, const int *PA,
           int *SA, int i, int size) {
  int j, k;
  int v;
  int c, d, e;

  for(v = SA[i], c = Td[PA[v]]; (j = 2 * i + 1) < size; SA[i] = SA[k], i = k) {
    d = Td[PA[SA[k = j++]]];
    if(d < (e = Td[PA[SA[j]]])) { k = j; d = e; }
    if(d <= c) { break; }
  }
  SA[i] = v;
}

/* Simple top-down heapsort. */
static
void
ss_heapsort(const unsigned char *Td, const int *PA, int *SA, int size) {
  int i, m;
  int t;

  m = size;
  if((size % 2) == 0) {
    m--;
    if(Td[PA[SA[m / 2]]] < Td[PA[SA[m]]]) { SWAP(SA[m], SA[m / 2]); }
  }

  for(i = m / 2 - 1; 0 <= i; --i) { ss_fixdown(Td, PA, SA, i, m); }
  if((size % 2) == 0) { SWAP(SA[0], SA[m]); ss_fixdown(Td, PA, SA, 0, m); }
  for(i = m - 1; 0 < i; --i) {
    t = SA[0], SA[0] = SA[i];
    ss_fixdown(Td, PA, SA, 0, i);
    SA[i] = t;
  }
}


/*---------------------------------------------------------------------------*/

/* Returns the median of three elements. */
static INLINE
int *
ss_median3(const unsigned char *Td, const int *PA,
           int *v1, int *v2, int *v3) {
  int *t;
  if(Td[PA[*v1]] > Td[PA[*v2]]) { SWAP(v1, v2); }
  if(Td[PA[*v2]] > Td[PA[*v3]]) {
    if(Td[PA[*v1]] > Td[PA[*v3]]) { return v1; }
    else { return v3; }
  }
  return v2;
}

/* Returns the median of five elements. */
static INLINE
int *
ss_median5(const unsigned char *Td, const int *PA,
           int *v1, int *v2, int *v3, int *v4, int *v5) {
  int *t;
  if(Td[PA[*v2]] > Td[PA[*v3]]) { SWAP(v2, v3); }
  if(Td[PA[*v4]] > Td[PA[*v5]]) { SWAP(v4, v5); }
  if(Td[PA[*v2]] > Td[PA[*v4]]) { SWAP(v2, v4); SWAP(v3, v5); }
  if(Td[PA[*v1]] > Td[PA[*v3]]) { SWAP(v1, v3); }
  if(Td[PA[*v1]] > Td[PA[*v4]]) { SWAP(v1, v4); SWAP(v3, v5); }
  if(Td[PA[*v3]] > Td[PA[*v4]]) { return v4; }
  return v3;
}

/* Returns the pivot element. */
static INLINE
int *
ss_pivot(const unsigned char *Td, const int *PA, int *first, int *last) {
  int *middle;
  int t;

  t = last - first;
  middle = first + t / 2;

  if(t <= 512) {
    if(t <= 32) {
      return ss_median3(Td, PA, first, middle, last - 1);
    } else {
      t >>= 2;
      return ss_median5(Td, PA, first, first + t, middle, last - 1 - t, last - 1);
    }
  }
  t >>= 3;
  first  = ss_median3(Td, PA, first, first + t, first + (t << 1));
  middle = ss_median3(Td, PA, middle - t, middle, middle + t);
  last   = ss_median3(Td, PA, last - 1 - (t << 1), last - 1 - t, last - 1);
  return ss_median3(Td, PA, first, middle, last);
}


/*---------------------------------------------------------------------------*/

/* Binary partition for substrings. */
static INLINE
int *
ss_partition(const int *PA,
                    int *first, int *last, int depth) {
  int *a, *b;
  int t;
  for(a = first - 1, b = last;;) {
    for(; (++a < b) && ((PA[*a] + depth) >= (PA[*a + 1] + 1));) { *a = ~*a; }
    for(; (a < --b) && ((PA[*b] + depth) <  (PA[*b + 1] + 1));) { }
    if(b <= a) { break; }
    t = ~*b;
    *b = *a;
    *a = t;
  }
  if(first < a) { *first = ~*first; }
  return a;
}

/* Multikey introsort for medium size groups. */
static
void
ss_mintrosort(const unsigned char *T, const int *PA,
              int *first, int *last,
              int depth) {
#define STACK_SIZE SS_MISORT_STACKSIZE
  struct { int *a, *b, c; int d; } stack[STACK_SIZE];
  const unsigned char *Td;
  int *a, *b, *c, *d, *e, *f;
  int s, t;
  int ssize;
  int limit;
  int v, x = 0;

  for(ssize = 0, limit = ss_ilg(last - first);;) {

    if((last - first) <= SS_INSERTIONSORT_THRESHOLD) {
#if 1 < SS_INSERTIONSORT_THRESHOLD
      if(1 < (last - first)) { ss_insertionsort(T, PA, first, last, depth); }
#endif
      STACK_POP(first, last, depth, limit);
      continue;
    }

    Td = T + depth;
    if(limit-- == 0) { ss_heapsort(Td, PA, first, last - first); }
    if(limit < 0) {
      for(a = first + 1, v = Td[PA[*first]]; a < last; ++a) {
        if((x = Td[PA[*a]]) != v) {
          if(1 < (a - first)) { break; }
          v = x;
          first = a;
        }
      }
      if(Td[PA[*first] - 1] < v) {
        first = ss_partition(PA, first, a, depth);
      }
      if((a - first) <= (last - a)) {
        if(1 < (a - first)) {
          STACK_PUSH(a, last, depth, -1);
          last = a, depth += 1, limit = ss_ilg(a - first);
        } else {
          first = a, limit = -1;
        }
      } else {
        if(1 < (last - a)) {
          STACK_PUSH(first, a, depth + 1, ss_ilg(a - first));
          first = a, limit = -1;
        } else {
          last = a, depth += 1, limit = ss_ilg(a - first);
        }
      }
      continue;
    }

    /* choose pivot */
    a = ss_pivot(Td, PA, first, last);
    v = Td[PA[*a]];
    SWAP(*first, *a);

    /* partition */
    for(b = first; (++b < last) && ((x = Td[PA[*b]]) == v);) { }
    if(((a = b) < last) && (x < v)) {
      for(; (++b < last) && ((x = Td[PA[*b]]) <= v);) {
        if(x == v) { SWAP(*b, *a); ++a; }
      }
    }
    for(c = last; (b < --c) && ((x = Td[PA[*c]]) == v);) { }
    if((b < (d = c)) && (x > v)) {
      for(; (b < --c) && ((x = Td[PA[*c]]) >= v);) {
        if(x == v) { SWAP(*c, *d); --d; }
      }
    }
    for(; b < c;) {
      SWAP(*b, *c);
      for(; (++b < c) && ((x = Td[PA[*b]]) <= v);) {
        if(x == v) { SWAP(*b, *a); ++a; }
      }
      for(; (b < --c) && ((x = Td[PA[*c]]) >= v);) {
        if(x == v) { SWAP(*c, *d); --d; }
      }
    }

    if(a <= d) {
      c = b - 1;

      if((s = a - first) > (t = b - a)) { s = t; }
      for(e = first, f = b - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }
      if((s = d - c) > (t = last - d - 1)) { s = t; }
      for(e = b, f = last - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }

      a = first + (b - a), c = last - (d - c);
      b = (v <= Td[PA[*a] - 1]) ? a : ss_partition(PA, a, c, depth);

      if((a - first) <= (last - c)) {
        if((last - c) <= (c - b)) {
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          STACK_PUSH(c, last, depth, limit);
          last = a;
        } else if((a - first) <= (c - b)) {
          STACK_PUSH(c, last, depth, limit);
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          last = a;
        } else {
          STACK_PUSH(c, last, depth, limit);
          STACK_PUSH(first, a, depth, limit);
          first = b, last = c, depth += 1, limit = ss_ilg(c - b);
        }
      } else {
        if((a - first) <= (c - b)) {
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          STACK_PUSH(first, a, depth, limit);
          first = c;
        } else if((last - c) <= (c - b)) {
          STACK_PUSH(first, a, depth, limit);
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          first = c;
        } else {
          STACK_PUSH(first, a, depth, limit);
          STACK_PUSH(c, last, depth, limit);
          first = b, last = c, depth += 1, limit = ss_ilg(c - b);
        }
      }
    } else {
      limit += 1;
      if(Td[PA[*first] - 1] < v) {
        first = ss_partition(PA, first, last, depth);
        limit = ss_ilg(last - first);
      }
      depth += 1;
    }
  }
#undef STACK_SIZE
}

#endif /* (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE) */


/*---------------------------------------------------------------------------*/

#if SS_BLOCKSIZE != 0

static INLINE
void
ss_blockswap(int *a, int *b, int n) {
  int t;
  for(; 0 < n; --n, ++a, ++b) {
    t = *a, *a = *b, *b = t;
  }
}

static INLINE
void
ss_rotate(int *first, int *middle, int *last) {
  int *a, *b, t;
  int l, r;
  l = middle - first, r = last - middle;
  for(; (0 < l) && (0 < r);) {
    if(l == r) { ss_blockswap(first, middle, l); break; }
    if(l < r) {
      a = last - 1, b = middle - 1;
      t = *a;
      do {
        *a-- = *b, *b-- = *a;
        if(b < first) {
          *a = t;
          last = a;
          if((r -= l + 1) <= l) { break; }
          a -= 1, b = middle - 1;
          t = *a;
        }
      } while(1);
    } else {
      a = first, b = middle;
      t = *a;
      do {
        *a++ = *b, *b++ = *a;
        if(last <= b) {
          *a = t;
          first = a + 1;
          if((l -= r + 1) <= r) { break; }
          a += 1, b = middle;
          t = *a;
        }
      } while(1);
    }
  }
}


/*---------------------------------------------------------------------------*/

static
void
ss_inplacemerge(const unsigned char *T, const int *PA,
                int *first, int *middle, int *last,
                int depth) {
  const int *p;
  int *a, *b;
  int len, half;
  int q, r;
  int x;

  for(;;) {
    if(*(last - 1) < 0) { x = 1; p = PA + ~*(last - 1); }
    else                { x = 0; p = PA +  *(last - 1); }
    for(a = first, len = middle - first, half = len >> 1, r = -1;
        0 < len;
        len = half, half >>= 1) {
      b = a + half;
      q = ss_compare(T, PA + ((0 <= *b) ? *b : ~*b), p, depth);
      if(q < 0) {
        a = b + 1;
        half -= (len & 1) ^ 1;
      } else {
        r = q;
      }
    }
    if(a < middle) {
      if(r == 0) { *a = ~*a; }
      ss_rotate(a, middle, last);
      last -= middle - a;
      middle = a;
      if(first == middle) { break; }
    }
    --last;
    if(x != 0) { while(*--last < 0) { } }
    if(middle == last) { break; }
  }
}


/*---------------------------------------------------------------------------*/

/* Merge-forward with internal buffer. */
static
void
ss_mergeforward(const unsigned char *T, const int *PA,
                int *first, int *middle, int *last,
                int *buf, int depth) {
  int *a, *b, *c, *bufend;
  int t;
  int r;

  bufend = buf + (middle - first) - 1;
  ss_blockswap(buf, first, middle - first);

  for(t = *(a = first), b = buf, c = middle;;) {
    r = ss_compare(T, PA + *b, PA + *c, depth);
    if(r < 0) {
      do {
        *a++ = *b;
        if(bufend <= b) { *bufend = t; return; }
        *b++ = *a;
      } while(*b < 0);
    } else if(r > 0) {
      do {
        *a++ = *c, *c++ = *a;
        if(last <= c) {
          while(b < bufend) { *a++ = *b, *b++ = *a; }
          *a = *b, *b = t;
          return;
        }
      } while(*c < 0);
    } else {
      *c = ~*c;
      do {
        *a++ = *b;
        if(bufend <= b) { *bufend = t; return; }
        *b++ = *a;
      } while(*b < 0);

      do {
        *a++ = *c, *c++ = *a;
        if(last <= c) {
          while(b < bufend) { *a++ = *b, *b++ = *a; }
          *a = *b, *b = t;
          return;
        }
      } while(*c < 0);
    }
  }
}

/* Merge-backward with internal buffer. */
static
void
ss_mergebackward(const unsigned char *T, const int *PA,
                 int *first, int *middle, int *last,
                 int *buf, int depth) {
  const int *p1, *p2;
  int *a, *b, *c, *bufend;
  int t;
  int r;
  int x;

  bufend = buf + (last - middle) - 1;
  ss_blockswap(buf, middle, last - middle);

  x = 0;
  if(*bufend < 0)       { p1 = PA + ~*bufend; x |= 1; }
  else                  { p1 = PA +  *bufend; }
  if(*(middle - 1) < 0) { p2 = PA + ~*(middle - 1); x |= 2; }
  else                  { p2 = PA +  *(middle - 1); }
  for(t = *(a = last - 1), b = bufend, c = middle - 1;;) {
    r = ss_compare(T, p1, p2, depth);
    if(0 < r) {
      if(x & 1) { do { *a-- = *b, *b-- = *a; } while(*b < 0); x ^= 1; }
      *a-- = *b;
      if(b <= buf) { *buf = t; break; }
      *b-- = *a;
      if(*b < 0) { p1 = PA + ~*b; x |= 1; }
      else       { p1 = PA +  *b; }
    } else if(r < 0) {
      if(x & 2) { do { *a-- = *c, *c-- = *a; } while(*c < 0); x ^= 2; }
      *a-- = *c, *c-- = *a;
      if(c < first) {
        while(buf < b) { *a-- = *b, *b-- = *a; }
        *a = *b, *b = t;
        break;
      }
      if(*c < 0) { p2 = PA + ~*c; x |= 2; }
      else       { p2 = PA +  *c; }
    } else {
      if(x & 1) { do { *a-- = *b, *b-- = *a; } while(*b < 0); x ^= 1; }
      *a-- = ~*b;
      if(b <= buf) { *buf = t; break; }
      *b-- = *a;
      if(x & 2) { do { *a-- = *c, *c-- = *a; } while(*c < 0); x ^= 2; }
      *a-- = *c, *c-- = *a;
      if(c < first) {
        while(buf < b) { *a-- = *b, *b-- = *a; }
        *a = *b, *b = t;
        break;
      }
      if(*b < 0) { p1 = PA + ~*b; x |= 1; }
      else       { p1 = PA +  *b; }
      if(*c < 0) { p2 = PA + ~*c; x |= 2; }
      else       { p2 = PA +  *c; }
    }
  }
}

/* D&C based merge. */
static
void
ss_swapmerge(const unsigned char *T, const int *PA,
             int *first, int *middle, int *last,
             int *buf, int bufsize, int depth) {
#define STACK_SIZE SS_SMERGE_STACKSIZE
#define GETIDX(a) ((0 <= (a)) ? (a) : (~(a)))
#define MERGE_CHECK(a, b, c)\
  do {\
    if(((c) & 1) ||\
       (((c) & 2) && (ss_compare(T, PA + GETIDX(*((a) - 1)), PA + *(a), depth) == 0))) {\
      *(a) = ~*(a);\
    }\
    if(((c) & 4) && ((ss_compare(T, PA + GETIDX(*((b) - 1)), PA + *(b), depth) == 0))) {\
      *(b) = ~*(b);\
    }\
  } while(0)
  struct { int *a, *b, *c; int d; } stack[STACK_SIZE];
  int *l, *r, *lm, *rm;
  int m, len, half;
  int ssize;
  int check, next;

  for(check = 0, ssize = 0;;) {
    if((last - middle) <= bufsize) {
      if((first < middle) && (middle < last)) {
        ss_mergebackward(T, PA, first, middle, last, buf, depth);
      }
      MERGE_CHECK(first, last, check);
      STACK_POP(first, middle, last, check);
      continue;
    }

    if((middle - first) <= bufsize) {
      if(first < middle) {
        ss_mergeforward(T, PA, first, middle, last, buf, depth);
      }
      MERGE_CHECK(first, last, check);
      STACK_POP(first, middle, last, check);
      continue;
    }

    for(m = 0, len = MIN(middle - first, last - middle), half = len >> 1;
        0 < len;
        len = half, half >>= 1) {
      if(ss_compare(T, PA + GETIDX(*(middle + m + half)),
                       PA + GETIDX(*(middle - m - half - 1)), depth) < 0) {
        m += half + 1;
        half -= (len & 1) ^ 1;
      }
    }

    if(0 < m) {
      lm = middle - m, rm = middle + m;
      ss_blockswap(lm, middle, m);
      l = r = middle, next = 0;
      if(rm < last) {
        if(*rm < 0) {
          *rm = ~*rm;
          if(first < lm) { for(; *--l < 0;) { } next |= 4; }
          next |= 1;
        } else if(first < lm) {
          for(; *r < 0; ++r) { }
          next |= 2;
        }
      }

      if((l - first) <= (last - r)) {
        STACK_PUSH(r, rm, last, (next & 3) | (check & 4));
        middle = lm, last = l, check = (check & 3) | (next & 4);
      } else {
        if((next & 2) && (r == middle)) { next ^= 6; }
        STACK_PUSH(first, lm, l, (check & 3) | (next & 4));
        first = r, middle = rm, check = (next & 3) | (check & 4);
      }
    } else {
      if(ss_compare(T, PA + GETIDX(*(middle - 1)), PA + *middle, depth) == 0) {
        *middle = ~*middle;
      }
      MERGE_CHECK(first, last, check);
      STACK_POP(first, middle, last, check);
    }
  }
#undef STACK_SIZE
}

#endif /* SS_BLOCKSIZE != 0 */


/*---------------------------------------------------------------------------*/

/* Substring sort */
static
void
sssort(const unsigned char *T, const int *PA,
       int *first, int *last,
       int *buf, int bufsize,
       int depth, int n, int lastsuffix) {
  int *a;
#if SS_BLOCKSIZE != 0
  int *b, *middle, *curbuf;
  int j, k, curbufsize, limit;
#endif
  int i;

  if(lastsuffix != 0) { ++first; }

#if SS_BLOCKSIZE == 0
  ss_mintrosort(T, PA, first, last, depth);
#else
  if((bufsize < SS_BLOCKSIZE) &&
      (bufsize < (last - first)) &&
      (bufsize < (limit = ss_isqrt(last - first)))) {
    if(SS_BLOCKSIZE < limit) { limit = SS_BLOCKSIZE; }
    buf = middle = last - limit, bufsize = limit;
  } else {
    middle = last, limit = 0;
  }
  for(a = first, i = 0; SS_BLOCKSIZE < (middle - a); a += SS_BLOCKSIZE, ++i) {
#if SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE
    ss_mintrosort(T, PA, a, a + SS_BLOCKSIZE, depth);
#elif 1 < SS_BLOCKSIZE
    ss_insertionsort(T, PA, a, a + SS_BLOCKSIZE, depth);
#endif
    curbufsize = last - (a + SS_BLOCKSIZE);
    curbuf = a + SS_BLOCKSIZE;
    if(curbufsize <= bufsize) { curbufsize = bufsize, curbuf = buf; }
    for(b = a, k = SS_BLOCKSIZE, j = i; j & 1; b -= k, k <<= 1, j >>= 1) {
      ss_swapmerge(T, PA, b - k, b, b + k, curbuf, curbufsize, depth);
    }
  }
#if SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE
  ss_mintrosort(T, PA, a, middle, depth);
#elif 1 < SS_BLOCKSIZE
  ss_insertionsort(T, PA, a, middle, depth);
#endif
  for(k = SS_BLOCKSIZE; i != 0; k <<= 1, i >>= 1) {
    if(i & 1) {
      ss_swapmerge(T, PA, a - k, a, middle, buf, bufsize, depth);
      a -= k;
    }
  }
  if(limit != 0) {
#if SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE
    ss_mintrosort(T, PA, middle, last, depth);
#elif 1 < SS_BLOCKSIZE
    ss_insertionsort(T, PA, middle, last, depth);
#endif
    ss_inplacemerge(T, PA, first, middle, last, depth);
  }
#endif

  if(lastsuffix != 0) {
    /* Insert last type B* suffix. */
    int PAi[2]; PAi[0] = PA[*(first - 1)], PAi[1] = n - 2;
    for(a = first, i = *(first - 1);
        (a < last) && ((*a < 0) || (0 < ss_compare(T, &(PAi[0]), PA + *a, depth)));
        ++a) {
      *(a - 1) = *a;
    }
    *(a - 1) = i;
  }
}


/*---------------------------------------------------------------------------*/

static INLINE
int
tr_ilg(int n) {
  return (n & 0xffff0000) ?
          ((n & 0xff000000) ?
            24 + lg_table[(n >> 24) & 0xff] :
            16 + lg_table[(n >> 16) & 0xff]) :
          ((n & 0x0000ff00) ?
             8 + lg_table[(n >>  8) & 0xff] :
             0 + lg_table[(n >>  0) & 0xff]);
}


/*---------------------------------------------------------------------------*/

/* Simple insertionsort for small size groups. */
static
void
tr_insertionsort(const int *ISAd, int *first, int *last) {
  int *a, *b;
  int t, r;

  for(a = first + 1; a < last; ++a) {
    for(t = *a, b = a - 1; 0 > (r = ISAd[t] - ISAd[*b]);) {
      do { *(b + 1) = *b; } while((first <= --b) && (*b < 0));
      if(b < first) { break; }
    }
    if(r == 0) { *b = ~*b; }
    *(b + 1) = t;
  }
}


/*---------------------------------------------------------------------------*/

static INLINE
void
tr_fixdown(const int *ISAd, int *SA, int i, int size) {
  int j, k;
  int v;
  int c, d, e;

  for(v = SA[i], c = ISAd[v]; (j = 2 * i + 1) < size; SA[i] = SA[k], i = k) {
    d = ISAd[SA[k = j++]];
    if(d < (e = ISAd[SA[j]])) { k = j; d = e; }
    if(d <= c) { break; }
  }
  SA[i] = v;
}

/* Simple top-down heapsort. */
static
void
tr_heapsort(const int *ISAd, int *SA, int size) {
  int i, m;
  int t;

  m = size;
  if((size % 2) == 0) {
    m--;
    if(ISAd[SA[m / 2]] < ISAd[SA[m]]) { SWAP(SA[m], SA[m / 2]); }
  }

  for(i = m / 2 - 1; 0 <= i; --i) { tr_fixdown(ISAd, SA, i, m); }
  if((size % 2) == 0) { SWAP(SA[0], SA[m]); tr_fixdown(ISAd, SA, 0, m); }
  for(i = m - 1; 0 < i; --i) {
    t = SA[0], SA[0] = SA[i];
    tr_fixdown(ISAd, SA, 0, i);
    SA[i] = t;
  }
}


/*---------------------------------------------------------------------------*/

/* Returns the median of three elements. */
static INLINE
int *
tr_median3(const int *ISAd, int *v1, int *v2, int *v3) {
  int *t;
  if(ISAd[*v1] > ISAd[*v2]) { SWAP(v1, v2); }
  if(ISAd[*v2] > ISAd[*v3]) {
    if(ISAd[*v1] > ISAd[*v3]) { return v1; }
    else { return v3; }
  }
  return v2;
}

/* Returns the median of five elements. */
static INLINE
int *
tr_median5(const int *ISAd,
           int *v1, int *v2, int *v3, int *v4, int *v5) {
  int *t;
  if(ISAd[*v2] > ISAd[*v3]) { SWAP(v2, v3); }
  if(ISAd[*v4] > ISAd[*v5]) { SWAP(v4, v5); }
  if(ISAd[*v2] > ISAd[*v4]) { SWAP(v2, v4); SWAP(v3, v5); }
  if(ISAd[*v1] > ISAd[*v3]) { SWAP(v1, v3); }
  if(ISAd[*v1] > ISAd[*v4]) { SWAP(v1, v4); SWAP(v3, v5); }
  if(ISAd[*v3] > ISAd[*v4]) { return v4; }
  return v3;
}

/* Returns the pivot element. */
static INLINE
int *
tr_pivot(const int *ISAd, int *first, int *last) {
  int *middle;
  int t;

  t = last - first;
  middle = first + t / 2;

  if(t <= 512) {
    if(t <= 32) {
      return tr_median3(ISAd, first, middle, last - 1);
    } else {
      t >>= 2;
      return tr_median5(ISAd, first, first + t, middle, last - 1 - t, last - 1);
    }
  }
  t >>= 3;
  first  = tr_median3(ISAd, first, first + t, first + (t << 1));
  middle = tr_median3(ISAd, middle - t, middle, middle + t);
  last   = tr_median3(ISAd, last - 1 - (t << 1), last - 1 - t, last - 1);
  return tr_median3(ISAd, first, middle, last);
}


/*---------------------------------------------------------------------------*/

typedef struct _trbudget_t trbudget_t;
struct _trbudget_t {
  int chance;
  int remain;
  int incval;
  int count;
};

static INLINE
void
trbudget_init(trbudget_t *budget, int chance, int incval) {
  budget->chance = chance;
  budget->remain = budget->incval = incval;
}

static INLINE
int
trbudget_check(trbudget_t *budget, int size) {
  if(size <= budget->remain) { budget->remain -= size; return 1; }
  if(budget->chance == 0) { budget->count += size; return 0; }
  budget->remain += budget->incval - size;
  budget->chance -= 1;
  return 1;
}


/*---------------------------------------------------------------------------*/

static INLINE
void
tr_partition(const int *ISAd,
             int *first, int *middle, int *last,
             int **pa, int **pb, int v) {
  int *a, *b, *c, *d, *e, *f;
  int t, s;
  int x = 0;

  for(b = middle - 1; (++b < last) && ((x = ISAd[*b]) == v);) { }
  if(((a = b) < last) && (x < v)) {
    for(; (++b < last) && ((x = ISAd[*b]) <= v);) {
      if(x == v) { SWAP(*b, *a); ++a; }
    }
  }
  for(c = last; (b < --c) && ((x = ISAd[*c]) == v);) { }
  if((b < (d = c)) && (x > v)) {
    for(; (b < --c) && ((x = ISAd[*c]) >= v);) {
      if(x == v) { SWAP(*c, *d); --d; }
    }
  }
  for(; b < c;) {
    SWAP(*b, *c);
    for(; (++b < c) && ((x = ISAd[*b]) <= v);) {
      if(x == v) { SWAP(*b, *a); ++a; }
    }
    for(; (b < --c) && ((x = ISAd[*c]) >= v);) {
      if(x == v) { SWAP(*c, *d); --d; }
    }
  }

  if(a <= d) {
    c = b - 1;
    if((s = a - first) > (t = b - a)) { s = t; }
    for(e = first, f = b - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }
    if((s = d - c) > (t = last - d - 1)) { s = t; }
    for(e = b, f = last - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }
    first += (b - a), last -= (d - c);
  }
  *pa = first, *pb = last;
}

static
void
tr_copy(int *ISA, const int *SA,
        int *first, int *a, int *b, int *last,
        int depth) {
  /* sort suffixes of middle partition
     by using sorted order of suffixes of left and right partition. */
  int *c, *d, *e;
  int s, v;

  v = b - SA - 1;
  for(c = first, d = a - 1; c <= d; ++c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *++d = s;
      ISA[s] = d - SA;
    }
  }
  for(c = last - 1, e = d + 1, d = b; e < d; --c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *--d = s;
      ISA[s] = d - SA;
    }
  }
}

static
void
tr_partialcopy(int *ISA, const int *SA,
               int *first, int *a, int *b, int *last,
               int depth) {
  int *c, *d, *e;
  int s, v;
  int rank, lastrank, newrank = -1;

  v = b - SA - 1;
  lastrank = -1;
  for(c = first, d = a - 1; c <= d; ++c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *++d = s;
      rank = ISA[s + depth];
      if(lastrank != rank) { lastrank = rank; newrank = d - SA; }
      ISA[s] = newrank;
    }
  }

  lastrank = -1;
  for(e = d; first <= e; --e) {
    rank = ISA[*e];
    if(lastrank != rank) { lastrank = rank; newrank = e - SA; }
    if(newrank != rank) { ISA[*e] = newrank; }
  }

  lastrank = -1;
  for(c = last - 1, e = d + 1, d = b; e < d; --c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *--d = s;
      rank = ISA[s + depth];
      if(lastrank != rank) { lastrank = rank; newrank = d - SA; }
      ISA[s] = newrank;
    }
  }
}

static
void
tr_introsort(int *ISA, const int *ISAd,
             int *SA, int *first, int *last,
             trbudget_t *budget) {
#define STACK_SIZE TR_STACKSIZE
  struct { const int *a; int *b, *c; int d, e; }stack[STACK_SIZE];
  int *a, *b, *c;
  int t;
  int v, x = 0;
  int incr = ISAd - ISA;
  int limit, next;
  int ssize, trlink = -1;

  for(ssize = 0, limit = tr_ilg(last - first);;) {

    if(limit < 0) {
      if(limit == -1) {
        /* tandem repeat partition */
        tr_partition(ISAd - incr, first, first, last, &a, &b, last - SA - 1);

        /* update ranks */
        if(a < last) {
          for(c = first, v = a - SA - 1; c < a; ++c) { ISA[*c] = v; }
        }
        if(b < last) {
          for(c = a, v = b - SA - 1; c < b; ++c) { ISA[*c] = v; }
        }

        /* push */
        if(1 < (b - a)) {
          STACK_PUSH5(NULL, a, b, 0, 0);
          STACK_PUSH5(ISAd - incr, first, last, -2, trlink);
          trlink = ssize - 2;
        }
        if((a - first) <= (last - b)) {
          if(1 < (a - first)) {
            STACK_PUSH5(ISAd, b, last, tr_ilg(last - b), trlink);
            last = a, limit = tr_ilg(a - first);
          } else if(1 < (last - b)) {
            first = b, limit = tr_ilg(last - b);
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        } else {
          if(1 < (last - b)) {
            STACK_PUSH5(ISAd, first, a, tr_ilg(a - first), trlink);
            first = b, limit = tr_ilg(last - b);
          } else if(1 < (a - first)) {
            last = a, limit = tr_ilg(a - first);
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        }
      } else if(limit == -2) {
        /* tandem repeat copy */
        a = stack[--ssize].b, b = stack[ssize].c;
        if(stack[ssize].d == 0) {
          tr_copy(ISA, SA, first, a, b, last, ISAd - ISA);
        } else {
          if(0 <= trlink) { stack[trlink].d = -1; }
          tr_partialcopy(ISA, SA, first, a, b, last, ISAd - ISA);
        }
        STACK_POP5(ISAd, first, last, limit, trlink);
      } else {
        /* sorted partition */
        if(0 <= *first) {
          a = first;
          do { ISA[*a] = a - SA; } while((++a < last) && (0 <= *a));
          first = a;
        }
        if(first < last) {
          a = first; do { *a = ~*a; } while(*++a < 0);
          next = (ISA[*a] != ISAd[*a]) ? tr_ilg(a - first + 1) : -1;
          if(++a < last) { for(b = first, v = a - SA - 1; b < a; ++b) { ISA[*b] = v; } }

          /* push */
          if(trbudget_check(budget, a - first)) {
            if((a - first) <= (last - a)) {
              STACK_PUSH5(ISAd, a, last, -3, trlink);
              ISAd += incr, last = a, limit = next;
            } else {
              if(1 < (last - a)) {
                STACK_PUSH5(ISAd + incr, first, a, next, trlink);
                first = a, limit = -3;
              } else {
                ISAd += incr, last = a, limit = next;
              }
            }
          } else {
            if(0 <= trlink) { stack[trlink].d = -1; }
            if(1 < (last - a)) {
              first = a, limit = -3;
            } else {
              STACK_POP5(ISAd, first, last, limit, trlink);
            }
          }
        } else {
          STACK_POP5(ISAd, first, last, limit, trlink);
        }
      }
      continue;
    }

    if((last - first) <= TR_INSERTIONSORT_THRESHOLD) {
      tr_insertionsort(ISAd, first, last);
      limit = -3;
      continue;
    }

    if(limit-- == 0) {
      tr_heapsort(ISAd, first, last - first);
      for(a = last - 1; first < a; a = b) {
        for(x = ISAd[*a], b = a - 1; (first <= b) && (ISAd[*b] == x); --b) { *b = ~*b; }
      }
      limit = -3;
      continue;
    }

    /* choose pivot */
    a = tr_pivot(ISAd, first, last);
    SWAP(*first, *a);
    v = ISAd[*first];

    /* partition */
    tr_partition(ISAd, first, first + 1, last, &a, &b, v);
    if((last - first) != (b - a)) {
      next = (ISA[*a] != v) ? tr_ilg(b - a) : -1;

      /* update ranks */
      for(c = first, v = a - SA - 1; c < a; ++c) { ISA[*c] = v; }
      if(b < last) { for(c = a, v = b - SA - 1; c < b; ++c) { ISA[*c] = v; } }

      /* push */
      if((1 < (b - a)) && (trbudget_check(budget, b - a))) {
        if((a - first) <= (last - b)) {
          if((last - b) <= (b - a)) {
            if(1 < (a - first)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              STACK_PUSH5(ISAd, b, last, limit, trlink);
              last = a;
            } else if(1 < (last - b)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              first = b;
            } else {
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else if((a - first) <= (b - a)) {
            if(1 < (a - first)) {
              STACK_PUSH5(ISAd, b, last, limit, trlink);
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              last = a;
            } else {
              STACK_PUSH5(ISAd, b, last, limit, trlink);
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else {
            STACK_PUSH5(ISAd, b, last, limit, trlink);
            STACK_PUSH5(ISAd, first, a, limit, trlink);
            ISAd += incr, first = a, last = b, limit = next;
          }
        } else {
          if((a - first) <= (b - a)) {
            if(1 < (last - b)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              STACK_PUSH5(ISAd, first, a, limit, trlink);
              first = b;
            } else if(1 < (a - first)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              last = a;
            } else {
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else if((last - b) <= (b - a)) {
            if(1 < (last - b)) {
              STACK_PUSH5(ISAd, first, a, limit, trlink);
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              first = b;
            } else {
              STACK_PUSH5(ISAd, first, a, limit, trlink);
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else {
            STACK_PUSH5(ISAd, first, a, limit, trlink);
            STACK_PUSH5(ISAd, b, last, limit, trlink);
            ISAd += incr, first = a, last = b, limit = next;
          }
        }
      } else {
        if((1 < (b - a)) && (0 <= trlink)) { stack[trlink].d = -1; }
        if((a - first) <= (last - b)) {
          if(1 < (a - first)) {
            STACK_PUSH5(ISAd, b, last, limit, trlink);
            last = a;
          } else if(1 < (last - b)) {
            first = b;
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        } else {
          if(1 < (last - b)) {
            STACK_PUSH5(ISAd, first, a, limit, trlink);
            first = b;
          } else if(1 < (a - first)) {
            last = a;
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        }
      }
    } else {
      if(trbudget_check(budget, last - first)) {
        limit = tr_ilg(last - first), ISAd += incr;
      } else {
        if(0 <= trlink) { stack[trlink].d = -1; }
        STACK_POP5(ISAd, first, last, limit, trlink);
      }
    }
  }
#undef STACK_SIZE
}



/*---------------------------------------------------------------------------*/

/* Tandem repeat sort */
static
void
trsort(int *ISA, int *SA, int n, int depth) {
  int *ISAd;
  int *first, *last;
  trbudget_t budget;
  int t, skip, unsorted;

  trbudget_init(&budget, tr_ilg(n) * 2 / 3, n);
/*  trbudget_init(&budget, tr_ilg(n) * 3 / 4, n); */
  for(ISAd = ISA + depth; -n < *SA; ISAd += ISAd - ISA) {
    first = SA;
    skip = 0;
    unsorted = 0;
    do {
      if((t = *first) < 0) { first -= t; skip += t; }
      else {
        if(skip != 0) { *(first + skip) = skip; skip = 0; }
        last = SA + ISA[t] + 1;
        if(1 < (last - first)) {
          budget.count = 0;
          tr_introsort(ISA, ISAd, SA, first, last, &budget);
          if(budget.count != 0) { unsorted += budget.count; }
          else { skip = first - last; }
        } else if((last - first) == 1) {
          skip = -1;
        }
        first = last;
      }
    } while(first < (SA + n));
    if(skip != 0) { *(first + skip) = skip; }
    if(unsorted == 0) { break; }
  }
}


/*---------------------------------------------------------------------------*/

/* Sorts suffixes of type B*. */
static
int
sort_typeBstar(const unsigned char *T, int *SA,
               int *bucket_A, int *bucket_B,
               int n) {
  int *PAb, *ISAb, *buf;
  int i, j, k, t, m, bufsize;
  int c0, c1;

  /* Initialize bucket arrays. */
  for(i = 0; i < BUCKET_A_SIZE; ++i) { bucket_A[i] = 0; }
  for(i = 0; i < BUCKET_B_SIZE; ++i) { bucket_B[i] = 0; }

  /* Count the number of occurrences of the first one or two characters of each
     type A, B and B* suffix. Moreover, store the beginning position of all
     type B* suffixes into the array SA. */
  for(i = n - 1, m = n, c0 = T[n - 1]; 0 <= i;) {
    /* type A suffix. */
    do { ++BUCKET_A(c1 = c0); } while((0 <= --i) && ((c0 = T[i]) >= c1));
    if(0 <= i) {
      /* type B* suffix. */
      ++BUCKET_BSTAR(c0, c1);
      SA[--m] = i;
      /* type B suffix. */
      for(--i, c1 = c0; (0 <= i) && ((c0 = T[i]) <= c1); --i, c1 = c0) {
        ++BUCKET_B(c0, c1);
      }
    }
  }
  m = n - m;
/*
note:
  A type B* suffix is lexicographically smaller than a type B suffix that
  begins with the same first two characters.
*/

  /* Calculate the index of start/end point of each bucket. */
  for(c0 = 0, i = 0, j = 0; c0 < ALPHABET_SIZE; ++c0) {
    t = i + BUCKET_A(c0);
    BUCKET_A(c0) = i + j; /* start point */
    i = t + BUCKET_B(c0, c0);
    for(c1 = c0 + 1; c1 < ALPHABET_SIZE; ++c1) {
      j += BUCKET_BSTAR(c0, c1);
      BUCKET_BSTAR(c0, c1) = j; /* end point */
      i += BUCKET_B(c0, c1);
    }
  }

  if(0 < m) {
    /* Sort the type B* suffixes by their first two characters. */
    PAb = SA + n - m; ISAb = SA + m;
    for(i = m - 2; 0 <= i; --i) {
      t = PAb[i], c0 = T[t], c1 = T[t + 1];
      SA[--BUCKET_BSTAR(c0, c1)] = i;
    }
    t = PAb[m - 1], c0 = T[t], c1 = T[t + 1];
    SA[--BUCKET_BSTAR(c0, c1)] = m - 1;

    /* Sort the type B* substrings using sssort. */
    buf = SA + m, bufsize = n - (2 * m);
    for(c0 = ALPHABET_SIZE - 2, j = m; 0 < j; --c0) {
      for(c1 = ALPHABET_SIZE - 1; c0 < c1; j = i, --c1) {
        i = BUCKET_BSTAR(c0, c1);
        if(1 < (j - i)) {
          sssort(T, PAb, SA + i, SA + j,
                 buf, bufsize, 2, n, *(SA + i) == (m - 1));
        }
      }
    }

    /* Compute ranks of type B* substrings. */
    for(i = m - 1; 0 <= i; --i) {
      if(0 <= SA[i]) {
        j = i;
        do { ISAb[SA[i]] = i; } while((0 <= --i) && (0 <= SA[i]));
        SA[i + 1] = i - j;
        if(i <= 0) { break; }
      }
      j = i;
      do { ISAb[SA[i] = ~SA[i]] = j; } while(SA[--i] < 0);
      ISAb[SA[i]] = j;
    }

    /* Construct the inverse suffix array of type B* suffixes using trsort. */
    trsort(ISAb, SA, m, 1);

    /* Set the sorted order of tyoe B* suffixes. */
    for(i = n - 1, j = m, c0 = T[n - 1]; 0 <= i;) {
      for(--i, c1 = c0; (0 <= i) && ((c0 = T[i]) >= c1); --i, c1 = c0) { }
      if(0 <= i) {
        t = i;
        for(--i, c1 = c0; (0 <= i) && ((c0 = T[i]) <= c1); --i, c1 = c0) { }
        SA[ISAb[--j]] = ((t == 0) || (1 < (t - i))) ? t : ~t;
      }
    }

    /* Calculate the index of start/end point of each bucket. */
    BUCKET_B(ALPHABET_SIZE - 1, ALPHABET_SIZE - 1) = n; /* end point */
    for(c0 = ALPHABET_SIZE - 2, k = m - 1; 0 <= c0; --c0) {
      i = BUCKET_A(c0 + 1) - 1;
      for(c1 = ALPHABET_SIZE - 1; c0 < c1; --c1) {
        t = i - BUCKET_B(c0, c1);
        BUCKET_B(c0, c1) = i; /* end point */

        /* Move all type B* suffixes to the correct position. */
        for(i = t, j = BUCKET_BSTAR(c0, c1);
            j <= k;
            --i, --k) { SA[i] = SA[k]; }
      }
      BUCKET_BSTAR(c0, c0 + 1) = i - BUCKET_B(c0, c0) + 1; /* start point */
      BUCKET_B(c0, c0) = i; /* end point */
    }
  }

  return m;
}

/* Constructs the suffix array by using the sorted order of type B* suffixes. */
static
void
construct_SA(const unsigned char *T, int *SA,
             int *bucket_A, int *bucket_B,
             int n, int m) {
  int *i, *j, *k;
  int s;
  int c0, c1, c2;

  if(0 < m) {
    /* Construct the sorted order of type B suffixes by using
       the sorted order of type B* suffixes. */
    for(c1 = ALPHABET_SIZE - 2; 0 <= c1; --c1) {
      /* Scan the suffix array from right to left. */
      for(i = SA + BUCKET_BSTAR(c1, c1 + 1),
          j = SA + BUCKET_A(c1 + 1) - 1, k = NULL, c2 = -1;
          i <= j;
          --j) {
        if(0 < (s = *j)) {
          *j = ~s;
          c0 = T[--s];
          if((0 < s) && (T[s - 1] > c0)) { s = ~s; }
          if(c0 != c2) {
            if(0 <= c2) { BUCKET_B(c2, c1) = k - SA; }
            k = SA + BUCKET_B(c2 = c0, c1);
          }
          *k-- = s;
        } else {
          *j = ~s;
        }
      }
    }
  }

  /* Construct the suffix array by using
     the sorted order of type B suffixes. */
  k = SA + BUCKET_A(c2 = T[n - 1]);
  *k++ = (T[n - 2] < c2) ? ~(n - 1) : (n - 1);
  /* Scan the suffix array from left to right. */
  for(i = SA, j = SA + n; i < j; ++i) {
    if(0 < (s = *i)) {
      c0 = T[--s];
      if((s == 0) || (T[s - 1] < c0)) { s = ~s; }
      if(c0 != c2) {
        BUCKET_A(c2) = k - SA;
        k = SA + BUCKET_A(c2 = c0);
      }
      *k++ = s;
    } else {
      *i = ~s;
    }
  }
}

/* Constructs the burrows-wheeler transformed string directly
   by using the sorted order of type B* suffixes. */
static
int
construct_BWT(const unsigned char *T, int *SA,
              int *bucket_A, int *bucket_B,
              int n, int m) {
  int *i, *j, *k, *orig;
  int s;
  int c0, c1, c2;

  if(0 < m) {
    /* Construct the sorted order of type B suffixes by using
       the sorted order of type B* suffixes. */
    for(c1 = ALPHABET_SIZE - 2; 0 <= c1; --c1) {
      /* Scan the suffix array from right to left. */
      for(i = SA + BUCKET_BSTAR(c1, c1 + 1),
          j = SA + BUCKET_A(c1 + 1) - 1, k = NULL, c2 = -1;
          i <= j;
          --j) {
        if(0 < (s = *j)) {
          c0 = T[--s];
          *j = ~((int)c0);
          if((0 < s) && (T[s - 1] > c0)) { s = ~s; }
          if(c0 != c2) {
            if(0 <= c2) { BUCKET_B(c2, c1) = k - SA; }
            k = SA + BUCKET_B(c2 = c0, c1);
          }
          *k-- = s;
        } else if(s != 0) {
          *j = ~s;
        }
      }
    }
  }

  /* Construct the BWTed string by using
     the sorted order of type B suffixes. */
  k = SA + BUCKET_A(c2 = T[n - 1]);
  *k++ = (T[n - 2] < c2) ? ~((int)T[n - 2]) : (n - 1);
  /* Scan the suffix array from left to right. */
  for(i = SA, j = SA + n, orig = SA; i < j; ++i) {
    if(0 < (s = *i)) {
      c0 = T[--s];
      *i = c0;
      if((0 < s) && (T[s - 1] < c0)) { s = ~((int)T[s - 1]); }
      if(c0 != c2) {
        BUCKET_A(c2) = k - SA;
        k = SA + BUCKET_A(c2 = c0);
      }
      *k++ = s;
    } else if(s != 0) {
      *i = ~s;
    } else {
      orig = i;
    }
  }

  return orig - SA;
}


/*---------------------------------------------------------------------------*/

/*- Function -*/

int
divsufsort(const unsigned char *T, int *SA, int n) {
  int *bucket_A, *bucket_B;
  int m;
  int err = 0;

  /* Check arguments. */
  if((T == NULL) || (SA == NULL) || (n < 0)) { return -1; }
  else if(n == 0) { return 0; }
  else if(n == 1) { SA[0] = 0; return 0; }
  else if(n == 2) { m = (T[0] < T[1]); SA[m ^ 1] = 0, SA[m] = 1; return 0; }

  bucket_A = (int *)malloc(BUCKET_A_SIZE * sizeof(int));
  bucket_B = (int *)malloc(BUCKET_B_SIZE * sizeof(int));

  /* Suffixsort. */
  if((bucket_A != NULL) && (bucket_B != NULL)) {
    m = sort_typeBstar(T, SA, bucket_A, bucket_B, n);
    construct_SA(T, SA, bucket_A, bucket_B, n, m);
  } else {
    err = -2;
  }

  free(bucket_B);
  free(bucket_A);

  return err;
}

int
divbwt(const unsigned char *T, unsigned char *U, int *A, int n) {
  int *B;
  int *bucket_A, *bucket_B;
  int m, pidx, i;

  /* Check arguments. */
  if((T == NULL) || (U == NULL) || (n < 0)) { return -1; }
  else if(n <= 1) { if(n == 1) { U[0] = T[0]; } return n; }

  if((B = A) == NULL) { B = (int *)malloc((size_t)(n + 1) * sizeof(int)); }
  bucket_A = (int *)malloc(BUCKET_A_SIZE * sizeof(int));
  bucket_B = (int *)malloc(BUCKET_B_SIZE * sizeof(int));

  /* Burrows-Wheeler Transform. */
  if((B != NULL) && (bucket_A != NULL) && (bucket_B != NULL)) {
    m = sort_typeBstar(T, B, bucket_A, bucket_B, n);
    pidx = construct_BWT(T, B, bucket_A, bucket_B, n, m);

    /* Copy to output string. */
    U[0] = T[n - 1];
    for(i = 0; i < pidx; ++i) { U[i + 1] = (unsigned char)B[i]; }
    for(i += 1; i < n; ++i) { U[i] = (unsigned char)B[i]; }
    pidx += 1;
  } else {
    pidx = -2;
  }

  free(bucket_B);
  free(bucket_A);
  if(A == NULL) { free(B); }

  return pidx;
}

// End divsufsort.c

/////////////////////////////// add ///////////////////////////////////

// Convert non-negative decimal number x to string of at least n digits
std::string itos(int64_t x, int n=1) {
  std::string r;
  for (; x || n>0; x/=10, --n) r=std::string(1, '0'+x%10)+r;
  return r;
}

// E8E9 transform of buf[0..n-1] to improve compression of .exe and .dll.
// Patterns (E8|E9 xx xx xx 00|FF) at offset i replace the 3 middle
// bytes with x+i mod 2^24, LSB first, reading backward.
void e8e9(unsigned char* buf, int n) {
  for (int i=n-5; i>=0; --i) {
    if (((buf[i]&254)==0xe8) && ((buf[i+4]+1)&254)==0) {
      unsigned a=(buf[i+1]|buf[i+2]<<8|buf[i+3]<<16)+i;
      buf[i+1]=a;
      buf[i+2]=a>>8;
      buf[i+3]=a>>16;
    }
  }
}

// Encode inbuf to buf using LZ77. args are as follows:
// args[0] is log2 buffer size in MB.
// args[1] is level (1=var. length, 2=byte aligned lz77, 3=bwt) + 4 if E8E9.
// args[2] is the lz77 minimum match length and context order.
// args[3] is the lz77 higher context order to search first, or else 0.
// args[4] is the log2 hash bucket size (number of searches).
// args[5] is the log2 hash table size. If 21+args[0] then use a suffix array.
// args[6] is the secondary context look ahead
// sap is pointer to external suffix array of inbuf or 0. If supplied and
//   args[0]=5..7 then it is assumed that E8E9 was already applied to
//   both the input and sap and the input buffer is not modified.

class LZBuffer: public libzpaq::Reader {
  libzpaq::Array<unsigned> ht;// hash table, confirm in low bits, or SA+ISA
  const unsigned char* in;    // input pointer
  const int checkbits;        // hash confirmation size or lg(ISA size)
  const int level;            // 1=var length LZ77, 2=byte aligned LZ77, 3=BWT
  const unsigned htsize;      // size of hash table
  const unsigned n;           // input length
  unsigned i;                 // current location in in (0 <= i < n)
  const unsigned minMatch;    // minimum match length
  const unsigned minMatch2;   // second context order or 0 if not used
  const unsigned maxMatch;    // longest match length allowed
  const unsigned maxLiteral;  // longest literal length allowed
  const unsigned lookahead;   // second context look ahead
  unsigned h1, h2;            // low, high order context hashes of in[i..]
  const unsigned bucket;      // number of matches to search per hash - 1
  const unsigned shift1, shift2;  // how far to shift h1, h2 per hash
  const int minMatchBoth;     // max(minMatch, minMatch2)
  const unsigned rb;          // number of level 1 r bits in match code
  unsigned bits;              // pending output bits (level 1)
  unsigned nbits;             // number of bits in bits
  unsigned rpos, wpos;        // read, write pointers
  unsigned idx;               // BWT index
  const unsigned* sa;         // suffix array for BWT or LZ77-SA
  unsigned* isa;              // inverse suffix array for LZ77-SA
  enum {BUFSIZE=1<<14};       // output buffer size
  unsigned char buf[BUFSIZE]; // output buffer

  void write_literal(unsigned i, unsigned& lit);
  void write_match(unsigned len, unsigned off);
  void fill();  // encode to buf

  // write k bits of x
  void putb(unsigned x, int k) {
    x&=(1<<k)-1;
    bits|=x<<nbits;
    nbits+=k;
    while (nbits>7) {
      buf[wpos++]=bits, bits>>=8, nbits-=8;
    }
  }

  // write last byte
  void flush() {
    if (nbits>0) buf[wpos++]=bits;
    bits=nbits=0;
  }

  // write 1 byte
  void put(int c) {
    buf[wpos++]=c;
  }

public:
  LZBuffer(StringBuffer& inbuf, int args[], const unsigned* sap=0);

  // return 1 byte of compressed output (overrides Reader)
  int get() {
    int c=-1;
    if (rpos==wpos) fill();
    if (rpos<wpos) c=buf[rpos++];
    if (rpos==wpos) rpos=wpos=0;
    return c;
  }

  // Read up to p[0..n-1] and return bytes read.
  int read(char* p, int n);
};

// LZ/BWT preprocessor for levels 1..3 compression and e8e9 filter.
// Level 1 uses variable length LZ77 codes like in the lazy compressor:
//
//   00,n,L[n] = n literal bytes
//   mm,mmm,n,ll,r,q (mm > 00) = match 4*n+ll at offset (q<<rb)+r-1
//
// where q is written in 8mm+mmm-8 (0..23) bits with an implied leading 1 bit
// and n is written using interleaved Elias Gamma coding, i.e. the leading
// 1 bit is implied, remaining bits are preceded by a 1 and terminated by
// a 0. e.g. abc is written 1,b,1,c,0. Codes are packed LSB first and
// padded with leading 0 bits in the last byte. r is a number with rb bits,
// where rb = log2(blocksize) - 24.
//
// Level 2 is byte oriented LZ77 with minimum match length m = $4 = args[3]
// with m in 1..64. Lengths and offsets are MSB first:
// 00xxxxxx   x+1 (1..64) literals follow
// yyxxxxxx   y+1 (2..4) offset bytes follow, match length x+m (m..m+63)
//
// Level 3 is BWT with the end of string byte coded as 255 and the
// last 4 bytes giving its position LSB first.

// floor(log2(x)) + 1 = number of bits excluding leading zeros (0..32)
int lg(unsigned x) {
  unsigned r=0;
  if (x>=65536) r=16, x>>=16;
  if (x>=256) r+=8, x>>=8;
  if (x>=16) r+=4, x>>=4;
  return
    "\x00\x01\x02\x02\x03\x03\x03\x03\x04\x04\x04\x04\x04\x04\x04\x04"[x]+r;
}

// return number of 1 bits in x
int nbits(unsigned x) {
  int r;
  for (r=0; x; x>>=1) r+=x&1;
  return r;
}

// Read n bytes of compressed output into p and return number of
// bytes read in 0..n. 0 signals EOF (overrides Reader).
int LZBuffer::read(char* p, int n) {
  if (rpos==wpos) fill();
  int nr=n;
  if (nr>int(wpos-rpos)) nr=wpos-rpos;
  if (nr) memcpy(p, buf+rpos, nr);
  rpos+=nr;
  if (rpos==wpos) rpos=wpos=0;
  return nr;
}

LZBuffer::LZBuffer(StringBuffer& inbuf, int args[], const unsigned* sap):
    ht((args[1]&3)==3 ? (inbuf.size()+1)*!sap      // for BWT suffix array
        : args[5]-args[0]<21 ? 1u<<args[5]         // for LZ77 hash table
        : (inbuf.size()*!sap)+(1u<<17<<args[0])),  // for LZ77 SA and ISA
    in(inbuf.data()),
    checkbits(args[5]-args[0]<21 ? 12-args[0] : 17+args[0]),
    level(args[1]&3),
    htsize(ht.size()),
    n(inbuf.size()),
    i(0),
    minMatch(args[2]),
    minMatch2(args[3]),
    maxMatch(BUFSIZE*3),
    maxLiteral(BUFSIZE/4),
    lookahead(args[6]),
    h1(0), h2(0),
    bucket((1<<args[4])-1), 
    shift1(minMatch>0 ? (args[5]-1)/minMatch+1 : 1),
    shift2(minMatch2>0 ? (args[5]-1)/minMatch2+1 : 0),
    minMatchBoth(MAX(minMatch, minMatch2+lookahead)+4),
    rb(args[0]>4 ? args[0]-4 : 0),
    bits(0), nbits(0), rpos(0), wpos(0),
    idx(0), sa(0), isa(0) {
  if ((minMatch<4 && level==1) || (minMatch<1 && level==2))
    error("match length $3 too small");

  // e8e9 transform
  if (args[1]>4 && !sap) e8e9(inbuf.data(), n);

  // build suffix array if not supplied
  if (args[5]-args[0]>=21 || level==3) {  // LZ77-SA or BWT
    if (sap)
      sa=sap;
    else {
      sa=&ht[0];
      if (n>0) divsufsort((const unsigned char*)in, (int*)sa, n);
    }
    if (level<3) {
      isa=&ht[n*(sap==0)];
    }
  }
}

// Encode from in to buf until end of input or buf is not empty
void LZBuffer::fill() {

  // BWT
  if (level==3) {
    for (; wpos<BUFSIZE && i<n+5; ++i) {
      if (i==0) put(n>0 ? in[n-1] : 255);
      else if (i>n) put(idx&255), idx>>=8;
      else if (sa[i-1]==0) idx=i, put(255);
      else put(in[sa[i-1]-1]);
    }
    return;
  }

  // LZ77: scan the input
  unsigned lit=0;  // number of output literals pending
  const unsigned mask=(1<<checkbits)-1;
  while (i<n && wpos*2<BUFSIZE) {

    // Search for longest match, or pick closest in case of tie
    unsigned blen=minMatch-1;  // best match length
    unsigned bp=0;  // pointer to best match
    unsigned blit=0;  // literals before best match
    int bscore=0;  // best cost

    // Look up contexts in suffix array
    if (isa) {
      if (sa[isa[i&mask]]!=i) // rebuild ISA
        for (unsigned j=0; j<n; ++j)
          if ((sa[j]&~mask)==(i&~mask))
            isa[sa[j]&mask]=j;
      for (unsigned h=0; h<=lookahead; ++h) {
        unsigned q=isa[(h+i)&mask];  // location of h+i in SA
        if (sa[q]!=h+i) continue;
        for (int j=-1; j<=1; j+=2) {  // search backward and forward
          for (unsigned k=1; k<=bucket; ++k) {
            unsigned p;  // match to be tested
            if (q+j*k<n && (p=sa[q+j*k]-h)<i) {
              unsigned l, l1;  // length of match, leading literals
              for (l=h; i+l<n && l<maxMatch && in[p+l]==in[i+l]; ++l);
              for (l1=h; l1>0 && in[p+l1-1]==in[i+l1-1]; --l1);
              int score=int(l-l1)*8-lg(i-p)-4*(lit==0 && l1>0)-11;
              for (unsigned a=0; a<h; ++a) score=score*5/8;
              if (score>bscore) blen=l, bp=p, blit=l1, bscore=score;
              if (l<blen || l<minMatch || l>255) break;
            }
          }
        }
        if (bscore<=0 || blen<minMatch) break;
      }
    }

    // Look up contexts in a hash table.
    // Try the longest context orders first. If a match is found, then
    // skip the lower order as a speed optimization.
    else if (level==1 || minMatch<=64) {
      if (minMatch2>0) {
        for (unsigned k=0; k<=bucket; ++k) {
          unsigned p=ht[h2^k];
          if (p && (p&mask)==(in[i+3]&mask)) {
            p>>=checkbits;
            if (p<i && i+blen<=n && in[p+blen-1]==in[i+blen-1]) {
              unsigned l;  // match length from lookahead
              for (l=lookahead; i+l<n && l<maxMatch && in[p+l]==in[i+l]; ++l);
              if (l>=minMatch2+lookahead) {
                int l1;  // length back from lookahead
                for (l1=lookahead; l1>0 && in[p+l1-1]==in[i+l1-1]; --l1);
                int score=int(l-l1)*8-lg(i-p)-8*(lit==0 && l1>0)-11;
                if (score>bscore) blen=l, bp=p, blit=l1, bscore=score;
              }
            }
          }
          if (blen>=128) break;
        }
      }

      // Search the lower order context
      if (!minMatch2 || blen<minMatch2) {
        for (unsigned k=0; k<=bucket; ++k) {
          unsigned p=ht[h1^k];
          if (p && i+3<n && (p&mask)==(in[i+3]&mask)) {
            p>>=checkbits;
            if (p<i && i+blen<=n && in[p+blen-1]==in[i+blen-1]) {
              unsigned l;
              for (l=0; i+l<n && l<maxMatch && in[p+l]==in[i+l]; ++l);
              int score=l*8-lg(i-p)-2*(lit>0)-11;
              if (score>bscore) blen=l, bp=p, blit=0, bscore=score;
            }
          }
          if (blen>=128) break;
        }
      }
    }

    // If match is long enough, then output any pending literals first,
    // and then the match. blen is the length of the match.
    const unsigned off=i-bp;  // offset
    if (off>0 && bscore>0
        && blen-blit>=minMatch+(level==2)*((off>=(1<<16))+(off>=(1<<24)))) {
      lit+=blit;
      write_literal(i+blit, lit);
      write_match(blen-blit, off);
    }

    // Otherwise add to literal length
    else {
      blen=1;
      ++lit;
    }

    // Update index, advance blen bytes
    if (isa)
      i+=blen;
    else {
      while (blen--) {
        if (i+minMatchBoth<n) {
          unsigned ih=((i*1234547)>>19)&bucket;
          const unsigned p=(i<<checkbits)|(in[i+3]&mask);
          if (minMatch2) {
            ht[h2^ih]=p;
            h2=(((h2*9)<<shift2)
                +(in[i+minMatch2+lookahead]+1)*23456789u)&(htsize-1);
          }
          ht[h1^ih]=p;
          h1=(((h1*5)<<shift1)+(in[i+minMatch]+1)*123456791u)&(htsize-1);
        }
        ++i;
      }
    }

    // Write long literals to keep buf from filling up
    if (lit>=maxLiteral)
      write_literal(i, lit);
  }

  // Write pending literals at end of input
  if (i==n) {
    write_literal(n, lit);
    flush();
  }
}

// Write literal sequence in[i-lit..i-1], set lit=0
void LZBuffer::write_literal(unsigned i, unsigned& lit) {
  if (level==1) {
    if (lit<1) return;
    int ll=lg(lit);
    putb(0, 2);
    --ll;
    while (--ll>=0) {
      putb(1, 1);
      putb((lit>>ll)&1, 1);
    }
    putb(0, 1);
    while (lit) putb(in[i-lit--], 8);
  }
  else {
    while (lit>0) {
      unsigned lit1=lit;
      if (lit1>64) lit1=64;
      put(lit1-1);
      for (unsigned j=i-lit; j<i-lit+lit1; ++j) put(in[j]);
      lit-=lit1;
    }
  }
}

// Write match sequence of given length and offset
void LZBuffer::write_match(unsigned len, unsigned off) {

  // mm,mmm,n,ll,r,q[mmmmm-8] = match n*4+ll, offset ((q-1)<<rb)+r+1
  if (level==1) {
    int ll=lg(len)-1;
    off+=(1<<rb)-1;
    int lo=lg(off)-1-rb;
    putb((lo+8)>>3, 2);// mm
    putb(lo&7, 3);     // mmm
    while (--ll>=2) {  // n
      putb(1, 1);
      putb((len>>ll)&1, 1);
    }
    putb(0, 1);
    putb(len&3, 2);    // ll
    putb(off, rb);     // r
    putb(off>>rb, lo); // q
  }

  // x[2]:len[6] off[x-1] 
  else {
    --off;
    while (len>0) {  // Split long matches to len1=minMatch..minMatch+63
      const unsigned len1=len>minMatch*2+63 ? minMatch+63 :
          len>minMatch+63 ? len-minMatch : len;
      if (off<(1<<16)) {
        put(64+len1-minMatch);
        put(off>>8);
        put(off);
      }
      else if (off<(1<<24)) {
        put(128+len1-minMatch);
        put(off>>16);
        put(off>>8);
        put(off);
      }
      else {
        put(192+len1-minMatch);
        put(off>>24);
        put(off>>16);
        put(off>>8);
        put(off);
      }
      len-=len1;
    }
  }
}

// Generate a config file from the method argument with syntax:
// {0|x|s|i}[N1[,N2]...][{ciamtswf<cfg>}[N1[,N2]]...]...
std::string makeConfig(const char* method, int args[]) {
  const char type=method[0];

  // Read "{x|s|i|0}N1,N2...N9" into args[0..8] ($1..$9)
  args[0]=0;  // log block size in MiB
  args[1]=0;  // 0=none, 1=var-LZ77, 2=byte-LZ77, 3=BWT, 4..7 adds E8E9
  args[2]=0;  // lz77 minimum match length
  args[3]=0;  // secondary context length
  args[4]=0;  // log searches
  args[5]=0;  // lz77 hash table size or SA if args[0]+21
  args[6]=0;  // secondary context look ahead
  args[7]=0;  // not used
  args[8]=0;  // not used
  if (isdigit(*++method)) args[0]=0;
  for (int i=0; i<9 && (isdigit(*method) || *method==',' || *method=='.');) {
    if (isdigit(*method))
      args[i]=args[i]*10+*method-'0';
    else if (++i<9)
      args[i]=0;
    ++method;
  }

  // "0..." = No compression
  if (type=='0')
    return "comp 0 0 0 0 0 hcomp end\n";

  // Generate the postprocessor
  std::string hdr, pcomp;
  const int level=args[1]&3;
  const bool doe8=args[1]>=4 && args[1]<=7;

  // LZ77+Huffman, with or without E8E9
  if (level==1) {
    const int rb=args[0]>4 ? args[0]-4 : 0;
    hdr="comp 9 16 0 $1+20 ";
    pcomp=
    "pcomp lazy2 3 ;\n"
    " (r1 = state\n"
    "  r2 = len - match or literal length\n"
    "  r3 = m - number of offset bits expected\n"
    "  r4 = ptr to buf\n"
    "  r5 = r - low bits of offset\n"
    "  c = bits - input buffer\n"
    "  d = n - number of bits in c)\n"
    "\n"
    "  a> 255 if\n";
    if (doe8)
      pcomp+=
      "    b=0 d=r 4 do (for b=0..d-1, d = end of buf)\n"
      "      a=b a==d ifnot\n"
      "        a+= 4 a<d if\n"
      "          a=*b a&= 254 a== 232 if (e8 or e9?)\n"
      "            c=b b++ b++ b++ b++ a=*b a++ a&= 254 a== 0 if (00 or ff)\n"
      "              b-- a=*b\n"
      "              b-- a<<= 8 a+=*b\n"
      "              b-- a<<= 8 a+=*b\n"
      "              a-=b a++\n"
      "              *b=a a>>= 8 b++\n"
      "              *b=a a>>= 8 b++\n"
      "              *b=a b++\n"
      "            endif\n"
      "            b=c\n"
      "          endif\n"
      "        endif\n"
      "        a=*b out b++\n"
      "      forever\n"
      "    endif\n"
      "\n";
    pcomp+=
    "    (reset state)\n"
    "    a=0 b=0 c=0 d=0 r=a 1 r=a 2 r=a 3 r=a 4\n"
    "    halt\n"
    "  endif\n"
    "\n"
    "  a<<=d a+=c c=a               (bits+=a<<n)\n"
    "  a= 8 a+=d d=a                (n+=8)\n"
    "\n"
    "  (if state==0 (expect new code))\n"
    "  a=r 1 a== 0 if (match code mm,mmm)\n"
    "    a= 1 r=a 2                 (len=1)\n"
    "    a=c a&= 3 a> 0 if          (if (bits&3))\n"
    "      a-- a<<= 3 r=a 3           (m=((bits&3)-1)*8)\n"
    "      a=c a>>= 2 c=a             (bits>>=2)\n"
    "      b=r 3 a&= 7 a+=b r=a 3     (m+=bits&7)\n"
    "      a=c a>>= 3 c=a             (bits>>=3)\n"
    "      a=d a-= 5 d=a              (n-=5)\n"
    "      a= 1 r=a 1                 (state=1)\n"
    "    else (literal, discard 00)\n"
    "      a=c a>>= 2 c=a             (bits>>=2)\n"
    "      d-- d--                    (n-=2)\n"
    "      a= 3 r=a 1                 (state=3)\n"
    "    endif\n"
    "  endif\n"
    "\n"
    "  (while state==1 && n>=3 (expect match length n*4+ll -> r2))\n"
    "  do a=r 1 a== 1 if a=d a> 2 if\n"
    "    a=c a&= 1 a== 1 if         (if bits&1)\n"
    "      a=c a>>= 1 c=a             (bits>>=1)\n"
    "      b=r 2 a=c a&= 1 a+=b a+=b r=a 2 (len+=len+(bits&1))\n"
    "      a=c a>>= 1 c=a             (bits>>=1)\n"
    "      d-- d--                    (n-=2)\n"
    "    else\n"
    "      a=c a>>= 1 c=a             (bits>>=1)\n"
    "      a=r 2 a<<= 2 b=a           (len<<=2)\n"
    "      a=c a&= 3 a+=b r=a 2       (len+=bits&3)\n"
    "      a=c a>>= 2 c=a             (bits>>=2)\n"
    "      d-- d-- d--                (n-=3)\n";
    if (rb)
      pcomp+="      a= 5 r=a 1                 (state=5)\n";
    else
      pcomp+="      a= 2 r=a 1                 (state=2)\n";
    pcomp+=
    "    endif\n"
    "  forever endif endif\n"
    "\n";
    if (rb) pcomp+=  // save r in r5
      "  (if state==5 && n>=8) (expect low bits of offset to put in r5)\n"
      "  a=r 1 a== 5 if a=d a> "+itos(rb-1)+" if\n"
      "    a=c a&= "+itos((1<<rb)-1)+" r=a 5            (save r in r5)\n"
      "    a=c a>>= "+itos(rb)+" c=a\n"
      "    a=d a-= "+itos(rb)+ " d=a\n"
      "    a= 2 r=a 1                   (go to state 2)\n"
      "  endif endif\n"
      "\n";
    pcomp+=
    "  (if state==2 && n>=m) (expect m offset bits)\n"
    "  a=r 1 a== 2 if a=r 3 a>d ifnot\n"
    "    a=c r=a 6 a=d r=a 7          (save c=bits, d=n in r6,r7)\n"
    "    b=r 3 a= 1 a<<=b d=a         (d=1<<m)\n"
    "    a-- a&=c a+=d                (d=offset=bits&((1<<m)-1)|(1<<m))\n";
    if (rb)
      pcomp+=  // insert r into low bits of d
      "    a<<= "+itos(rb)+" d=r 5 a+=d a-= "+itos((1<<rb)-1)+"\n";
    pcomp+=
    "    d=a b=r 4 a=b a-=d c=a       (c=p=(b=ptr)-offset)\n"
    "\n"
    "    (while len-- (copy and output match d bytes from *c to *b))\n"
    "    d=r 2 do a=d a> 0 if d--\n"
    "      a=*c *b=a c++ b++          (buf[ptr++]-buf[p++])\n";
    if (!doe8) pcomp+=" out\n";
    pcomp+=
    "    forever endif\n"
    "    a=b r=a 4\n"
    "\n"
    "    a=r 6 b=r 3 a>>=b c=a        (bits>>=m)\n"
    "    a=r 7 a-=b d=a               (n-=m)\n"
    "    a=0 r=a 1                    (state=0)\n"
    "  endif endif\n"
    "\n"
    "  (while state==3 && n>=2 (expect literal length))\n"
    "  do a=r 1 a== 3 if a=d a> 1 if\n"
    "    a=c a&= 1 a== 1 if         (if bits&1)\n"
    "      a=c a>>= 1 c=a              (bits>>=1)\n"
    "      b=r 2 a&= 1 a+=b a+=b r=a 2 (len+=len+(bits&1))\n"
    "      a=c a>>= 1 c=a              (bits>>=1)\n"
    "      d-- d--                     (n-=2)\n"
    "    else\n"
    "      a=c a>>= 1 c=a              (bits>>=1)\n"
    "      d--                         (--n)\n"
    "      a= 4 r=a 1                  (state=4)\n"
    "    endif\n"
    "  forever endif endif\n"
    "\n"
    "  (if state==4 && n>=8 (expect len literals))\n"
    "  a=r 1 a== 4 if a=d a> 7 if\n"
    "    b=r 4 a=c *b=a\n";
    if (!doe8) pcomp+=" out\n";
    pcomp+=
    "    b++ a=b r=a 4                 (buf[ptr++]=bits)\n"
    "    a=c a>>= 8 c=a                (bits>>=8)\n"
    "    a=d a-= 8 d=a                 (n-=8)\n"
    "    a=r 2 a-- r=a 2 a== 0 if      (if --len<1)\n"
    "      a=0 r=a 1                     (state=0)\n"
    "    endif\n"
    "  endif endif\n"
    "  halt\n"
    "end\n";
  }

  // Byte aligned LZ77, with or without E8E9
  else if (level==2) {
    hdr="comp 9 16 0 $1+20 ";
    pcomp=
    "pcomp lzpre c ;\n"
    "  (Decode LZ77: d=state, M=output buffer, b=size)\n"
    "  a> 255 if (at EOF decode e8e9 and output)\n";
    if (doe8)
      pcomp+=
      "    d=b b=0 do (for b=0..d-1, d = end of buf)\n"
      "      a=b a==d ifnot\n"
      "        a+= 4 a<d if\n"
      "          a=*b a&= 254 a== 232 if (e8 or e9?)\n"
      "            c=b b++ b++ b++ b++ a=*b a++ a&= 254 a== 0 if (00 or ff)\n"
      "              b-- a=*b\n"
      "              b-- a<<= 8 a+=*b\n"
      "              b-- a<<= 8 a+=*b\n"
      "              a-=b a++\n"
      "              *b=a a>>= 8 b++\n"
      "              *b=a a>>= 8 b++\n"
      "              *b=a b++\n"
      "            endif\n"
      "            b=c\n"
      "          endif\n"
      "        endif\n"
      "        a=*b out b++\n"
      "      forever\n"
      "    endif\n";
    pcomp+=
    "    b=0 c=0 d=0 a=0 r=a 1 r=a 2 (reset state)\n"
    "  halt\n"
    "  endif\n"
    "\n"
    "  (in state d==0, expect a new code)\n"
    "  (put length in r1 and inital part of offset in r2)\n"
    "  c=a a=d a== 0 if\n"
    "    a=c a>>= 6 a++ d=a\n"
    "    a== 1 if (literal?)\n"
    "      a+=c r=a 1 a=0 r=a 2\n"
    "    else (3 to 5 byte match)\n"
    "      d++ a=c a&= 63 a+= $3 r=a 1 a=0 r=a 2\n"
    "    endif\n"
    "  else\n"
    "    a== 1 if (writing literal)\n"
    "      a=c *b=a b++\n";
    if (!doe8) pcomp+=" out\n";
    pcomp+=
    "      a=r 1 a-- a== 0 if d=0 endif r=a 1 (if (--len==0) state=0)\n"
    "    else\n"
    "      a> 2 if (reading offset)\n"
    "        a=r 2 a<<= 8 a|=c r=a 2 d-- (off=off<<8|c, --state)\n"
    "      else (state==2, write match)\n"
    "        a=r 2 a<<= 8 a|=c c=a a=b a-=c a-- c=a (c=i-off-1)\n"
    "        d=r 1 (d=len)\n"
    "        do (copy and output d=len bytes)\n"
    "          a=*c *b=a c++ b++\n";
    if (!doe8) pcomp+=" out\n";
    pcomp+=
    "        d-- a=d a> 0 while\n"
    "        (d=state=0. off, len don\'t matter)\n"
    "      endif\n"
    "    endif\n"
    "  endif\n"
    "  halt\n"
    "end\n";
  }

  // BWT with or without E8E9
  else if (level==3) {  // IBWT
    hdr="comp 9 16 $1+20 $1+20 ";  // 2^$1 = block size in MB
    pcomp=
    "pcomp bwtrle c ;\n"
    "\n"
    "  (read BWT, index into M, size in b)\n"
    "  a> 255 ifnot\n"
    "    *b=a b++\n"
    "\n"
    "  (inverse BWT)\n"
    "  elsel\n"
    "\n"
    "    (index in last 4 bytes, put in c and R1)\n"
    "    b-- a=*b\n"
    "    b-- a<<= 8 a+=*b\n"
    "    b-- a<<= 8 a+=*b\n"
    "    b-- a<<= 8 a+=*b c=a r=a 1\n"
    "\n"
    "    (save size in R2)\n"
    "    a=b r=a 2\n"
    "\n"
    "    (count bytes in H[~1..~255, ~0])\n"
    "    do\n"
    "      a=b a> 0 if\n"
    "        b-- a=*b a++ a&= 255 d=a d! *d++\n"
    "      forever\n"
    "    endif\n"
    "\n"
    "    (cumulative counts: H[~i=0..255] = count of bytes before i)\n"
    "    d=0 d! *d= 1 a=0\n"
    "    do\n"
    "      a+=*d *d=a d--\n"
    "    d<>a a! a> 255 a! d<>a until\n"
    "\n"
    "    (build first part of linked list in H[0..idx-1])\n"
    "    b=0 do\n"
    "      a=c a>b if\n"
    "        d=*b d! *d++ d=*d d-- *d=b\n"
    "      b++ forever\n"
    "    endif\n"
    "\n"
    "    (rest of list in H[idx+1..n-1])\n"
    "    b=c b++ c=r 2 do\n"
    "      a=c a>b if\n"
    "        d=*b d! *d++ d=*d d-- *d=b\n"
    "      b++ forever\n"
    "    endif\n"
    "\n";
    if (args[0]<=4) {  // faster IBWT list traversal limited to 16 MB blocks
      pcomp+=
      "    (copy M to low 8 bits of H to reduce cache misses in next loop)\n"
      "    b=0 do\n"
      "      a=c a>b if\n"
      "        d=b a=*d a<<= 8 a+=*b *d=a\n"
      "      b++ forever\n"
      "    endif\n"
      "\n"
      "    (traverse list and output or copy to M)\n"
      "    d=r 1 b=0 do\n"
      "      a=d a== 0 ifnot\n"
      "        a=*d a>>= 8 d=a\n";
      if (doe8) pcomp+=" *b=*d b++\n";
      else      pcomp+=" a=*d out\n";
      pcomp+=
      "      forever\n"
      "    endif\n"
      "\n";
      if (doe8)  // IBWT+E8E9
        pcomp+=
        "    (e8e9 transform to out)\n"
        "    d=b b=0 do (for b=0..d-1, d = end of buf)\n"
        "      a=b a==d ifnot\n"
        "        a+= 4 a<d if\n"
        "          a=*b a&= 254 a== 232 if\n"
        "            c=b b++ b++ b++ b++ a=*b a++ a&= 254 a== 0 if\n"
        "              b-- a=*b\n"
        "              b-- a<<= 8 a+=*b\n"
        "              b-- a<<= 8 a+=*b\n"
        "              a-=b a++\n"
        "              *b=a a>>= 8 b++\n"
        "              *b=a a>>= 8 b++\n"
        "              *b=a b++\n"
        "            endif\n"
        "            b=c\n"
        "          endif\n"
        "        endif\n"
        "        a=*b out b++\n"
        "      forever\n"
        "    endif\n";
      pcomp+=
      "  endif\n"
      "  halt\n"
      "end\n";
    }
    else {  // slower IBWT list traversal for all sized blocks
      if (doe8) {  // E8E9 after IBWT
        pcomp+=
        "    (R2 = output size without EOS)\n"
        "    a=r 2 a-- r=a 2\n"
        "\n"
        "    (traverse list (d = IBWT pointer) and output inverse e8e9)\n"
        "    (C = offset = 0..R2-1)\n"
        "    (R4 = last 4 bytes shifted in from MSB end)\n"
        "    (R5 = temp pending output byte)\n"
        "    c=0 d=r 1 do\n"
        "      a=d a== 0 ifnot\n"
        "        d=*d\n"
        "\n"
        "        (store byte in R4 and shift out to R5)\n"
        "        b=d a=*b a<<= 24 b=a\n"
        "        a=r 4 r=a 5 a>>= 8 a|=b r=a 4\n"
        "\n"
        "        (if E8|E9 xx xx xx 00|FF in R4:R5 then subtract c from x)\n"
        "        a=c a> 3 if\n"
        "          a=r 5 a&= 254 a== 232 if\n"
        "            a=r 4 a>>= 24 b=a a++ a&= 254 a< 2 if\n"
        "              a=r 4 a-=c a+= 4 a<<= 8 a>>= 8 \n"
        "              b<>a a<<= 24 a+=b r=a 4\n"
        "            endif\n"
        "          endif\n"
        "        endif\n"
        "\n"
        "        (output buffered byte)\n"
        "        a=c a> 3 if a=r 5 out endif c++\n"
        "\n"
        "      forever\n"
        "    endif\n"
        "\n"
        "    (output up to 4 pending bytes in R4)\n"
        "    b=r 4\n"
        "    a=c a> 3 a=b if out endif a>>= 8 b=a\n"
        "    a=c a> 2 a=b if out endif a>>= 8 b=a\n"
        "    a=c a> 1 a=b if out endif a>>= 8 b=a\n"
        "    a=c a> 0 a=b if out endif\n"
        "\n"
        "  endif\n"
        "  halt\n"
        "end\n";
      }
      else {
        pcomp+=
        "    (traverse list and output)\n"
        "    d=r 1 do\n"
        "      a=d a== 0 ifnot\n"
        "        d=*d\n"
        "        b=d a=*b out\n"
        "      forever\n"
        "    endif\n"
        "  endif\n"
        "  halt\n"
        "end\n";
      }
    }
  }

  // E8E9 or no preprocessing
  else if (level==0) {
    hdr="comp 9 16 0 0 ";
    if (doe8) { // E8E9?
      pcomp=
      "pcomp e8e9 d ;\n"
      "  a> 255 if\n"
      "    a=c a> 4 if\n"
      "      c= 4\n"
      "    else\n"
      "      a! a+= 5 a<<= 3 d=a a=b a>>=d b=a\n"
      "    endif\n"
      "    do a=c a> 0 if\n"
      "      a=b out a>>= 8 b=a c--\n"
      "    forever endif\n"
      "  else\n"
      "    *b=b a<<= 24 d=a a=b a>>= 8 a+=d b=a c++\n"
      "    a=c a> 4 if\n"
      "      a=*b out\n"
      "      a&= 254 a== 232 if\n"
      "        a=b a>>= 24 a++ a&= 254 a== 0 if\n"
      "          a=b a>>= 24 a<<= 24 d=a\n"
      "          a=b a-=c a+= 5\n"
      "          a<<= 8 a>>= 8 a|=d b=a\n"
      "        endif\n"
      "      endif\n"
      "    endif\n"
      "  endif\n"
      "  halt\n"
      "end\n";
    }
    else
      pcomp="end\n";
  }
  else
    error("Unsupported method");
  
  // Build context model (comp, hcomp) assuming:
  // H[0..254] = contexts
  // H[255..511] = location of last byte i-255
  // M = last 64K bytes, filling backward
  // C = pointer to most recent byte
  // R1 = level 2 lz77 1+bytes expected until next code, 0=init
  // R2 = level 2 lz77 first byte of code
  int ncomp=0;  // number of components
  const int membits=args[0]+20;
  int sb=5;  // bits in last context
  std::string comp;
  std::string hcomp="hcomp\n"
    "c-- *c=a a+= 255 d=a *d=c\n";
  if (level==2) {  // put level 2 lz77 parse state in R1, R2
    hcomp+=
    "  (decode lz77 into M. Codes:\n"
    "  00xxxxxx = literal length xxxxxx+1\n"
    "  xx......, xx > 0 = match with xx offset bytes to follow)\n"
    "\n"
    "  a=r 1 a== 0 if (init)\n"
    "    a= "+itos(111+57*doe8)+" (skip post code)\n"
    "  else a== 1 if  (new code?)\n"
    "    a=*c r=a 2  (save code in R2)\n"
    "    a> 63 if a>>= 6 a++ a++  (match)\n"
    "    else a++ a++ endif  (literal)\n"
    "  else (read rest of code)\n"
    "    a--\n"
    "  endif endif\n"
    "  r=a 1  (R1 = 1+expected bytes to next code)\n";
  }

  // Generate the context model
  while (*method && ncomp<254) {

    // parse command C[N1[,N2]...] into v = {C, N1, N2...}
    std::vector<int> v;
    v.push_back(*method++);
    if (isdigit(*method)) {
      v.push_back(*method++-'0');
      while (isdigit(*method) || *method==',' || *method=='.') {
        if (isdigit(*method))
          v.back()=v.back()*10+*method++-'0';
        else {
          v.push_back(0);
          ++method;
        }
      }
    }

    // c: context model
    // N1%1000: 0=ICM 1..256=CM limit N1-1
    // N1/1000: number of times to halve memory
    // N2: 1..255=offset mod N2. 1000..1255=distance to N2-1000
    // N3...: 0..255=byte mask + 256=lz77 state. 1000+=run of N3-1000 zeros.
    if (v[0]=='c') {
      while (v.size()<3) v.push_back(0);
      comp+=itos(ncomp)+" ";
      sb=11;  // count context bits
      if (v[2]<256) sb+=lg(v[2]);
      else sb+=6;
      for (unsigned i=3; i<v.size(); ++i)
        if (v[i]<512) sb+=nbits(v[i])*3/4;
      if (sb>membits) sb=membits;
      if (v[1]%1000==0) comp+="icm "+itos(sb-6-v[1]/1000)+"\n";
      else comp+="cm "+itos(sb-2-v[1]/1000)+" "+itos(v[1]%1000-1)+"\n";

      // special contexts
      hcomp+="d= "+itos(ncomp)+" *d=0\n";
      if (v[2]>1 && v[2]<=255) {  // periodic context
        if (lg(v[2])!=lg(v[2]-1))
          hcomp+="a=c a&= "+itos(v[2]-1)+" hashd\n";
        else
          hcomp+="a=c a%= "+itos(v[2])+" hashd\n";
      }
      else if (v[2]>=1000 && v[2]<=1255)  // distance context
        hcomp+="a= 255 a+= "+itos(v[2]-1000)+
               " d=a a=*d a-=c a> 255 if a= 255 endif d= "+
               itos(ncomp)+" hashd\n";

      // Masked context
      for (unsigned i=3; i<v.size(); ++i) {
        if (i==3) hcomp+="b=c ";
        if (v[i]==255)
          hcomp+="a=*b hashd\n";  // ordinary byte
        else if (v[i]>0 && v[i]<255)
          hcomp+="a=*b a&= "+itos(v[i])+" hashd\n";  // masked byte
        else if (v[i]>=256 && v[i]<512) { // lz77 state or masked literal byte
          hcomp+=
          "a=r 1 a> 1 if\n"  // expect literal or offset
          "  a=r 2 a< 64 if\n"  // expect literal
          "    a=*b ";
          if (v[i]<511) hcomp+="a&= "+itos(v[i]-256);
          hcomp+=" hashd\n"
          "  else\n"  // expect match offset byte
          "    a>>= 6 hashd a=r 1 hashd\n"
          "  endif\n"
          "else\n"  // expect new code
          "  a= 255 hashd a=r 2 hashd\n"
          "endif\n";
        }
        else if (v[i]>=1256)  // skip v[i]-1000 bytes
          hcomp+="a= "+itos(((v[i]-1000)>>8)&255)+" a<<= 8 a+= "
               +itos((v[i]-1000)&255)+
          " a+=b b=a\n";
        else if (v[i]>1000)
          hcomp+="a= "+itos(v[i]-1000)+" a+=b b=a\n";
        if (v[i]<512 && i<v.size()-1)
          hcomp+="b++ ";
      }
      ++ncomp;
    }

    // m,8,24: MIX, size, rate
    // t,8,24: MIX2, size, rate
    // s,8,32,255: SSE, size, start, limit
    if (strchr("mts", v[0]) && ncomp>int(v[0]=='t')) {
      if (v.size()<=1) v.push_back(8);
      if (v.size()<=2) v.push_back(24+8*(v[0]=='s'));
      if (v[0]=='s' && v.size()<=3) v.push_back(255);
      comp+=itos(ncomp);
      sb=5+v[1]*3/4;
      if (v[0]=='m')
        comp+=" mix "+itos(v[1])+" 0 "+itos(ncomp)+" "+itos(v[2])+" 255\n";
      else if (v[0]=='t')
        comp+=" mix2 "+itos(v[1])+" "+itos(ncomp-1)+" "+itos(ncomp-2)
            +" "+itos(v[2])+" 255\n";
      else // s
        comp+=" sse "+itos(v[1])+" "+itos(ncomp-1)+" "+itos(v[2])+" "
            +itos(v[3])+"\n";
      if (v[1]>8) {
        hcomp+="d= "+itos(ncomp)+" *d=0 b=c a=0\n";
        for (; v[1]>=16; v[1]-=8) {
          hcomp+="a<<= 8 a+=*b";
          if (v[1]>16) hcomp+=" b++";
          hcomp+="\n";
        }
        if (v[1]>8)
          hcomp+="a<<= 8 a+=*b a>>= "+itos(16-v[1])+"\n";
        hcomp+="a<<= 8 *d=a\n";
      }
      ++ncomp;
    }

    // i: ISSE chain with order increasing by N1,N2...
    if (v[0]=='i' && ncomp>0) {
      hcomp+="d= "+itos(ncomp-1)+" b=c a=*d d++\n";
      for (unsigned i=1; i<v.size() && ncomp<254; ++i) {
        for (int j=0; j<v[i]%10; ++j) {
          hcomp+="hash ";
          if (i<v.size()-1 || j<v[i]%10-1) hcomp+="b++ ";
          sb+=6;
        }
        hcomp+="*d=a";
        if (i<v.size()-1) hcomp+=" d++";
        hcomp+="\n";
        if (sb>membits) sb=membits;
        comp+=itos(ncomp)+" isse "+itos(sb-6-v[i]/10)+" "+itos(ncomp-1)+"\n";
        ++ncomp;
      }
    }

    // a24,0,0: MATCH. N1=hash multiplier. N2,N3=halve buf, table.
    if (v[0]=='a') {
      if (v.size()<=1) v.push_back(24);
      while (v.size()<4) v.push_back(0);
      comp+=itos(ncomp)+" match "+itos(membits-v[3]-2)+" "
          +itos(membits-v[2])+"\n";
      hcomp+="d= "+itos(ncomp)+" a=*d a*= "+itos(v[1])
           +" a+=*c a++ *d=a\n";
      sb=5+(membits-v[2])*3/4;
      ++ncomp;
    }

    // w1,65,26,223,20,0: ICM-ISSE chain of length N1 with word contexts,
    // where a word is a sequence of c such that c&N4 is in N2..N2+N3-1.
    // Word is hashed by: hash := hash*N5+c+1
    // Decrease memory by 2^-N6.
    if (v[0]=='w') {
      if (v.size()<=1) v.push_back(1);
      if (v.size()<=2) v.push_back(65);
      if (v.size()<=3) v.push_back(26);
      if (v.size()<=4) v.push_back(223);
      if (v.size()<=5) v.push_back(20);
      if (v.size()<=6) v.push_back(0);
      comp+=itos(ncomp)+" icm "+itos(membits-6-v[6])+"\n";
      for (int i=1; i<v[1]; ++i)
        comp+=itos(ncomp+i)+" isse "+itos(membits-6-v[6])+" "
            +itos(ncomp+i-1)+"\n";
      hcomp+="a=*c a&= "+itos(v[4])+" a-= "+itos(v[2])+" a&= 255 a< "
           +itos(v[3])+" if\n";
      for (int i=0; i<v[1]; ++i) {
        if (i==0) hcomp+="  d= "+itos(ncomp);
        else hcomp+="  d++";
        hcomp+=" a=*d a*= "+itos(v[5])+" a+=*c a++ *d=a\n";
      }
      hcomp+="else\n";
      for (int i=v[1]-1; i>0; --i)
        hcomp+="  d= "+itos(ncomp+i-1)+" a=*d d++ *d=a\n";
      hcomp+="  d= "+itos(ncomp)+" *d=0\n"
           "endif\n";
      ncomp+=v[1]-1;
      sb=membits-v[6];
      ++ncomp;
    }
  }
  return hdr+itos(ncomp)+"\n"+comp+hcomp+"halt\n"+pcomp;
}

// Compress from in to out in 1 segment in 1 block using the algorithm
// descried in method. If method begins with a digit then choose
// a method depending on type. Save filename and comment
// in the segment header. If comment is 0 then the default is the input size
// as a decimal string, plus " jDC\x01" for a journaling method (method[0]
// is not 's'). Write the generated method to methodOut if not 0.
void compressBlock(StringBuffer* in, Writer* out, const char* method_,
                   const char* filename, const char* comment, bool dosha1) {
  std::string method=method_;
  const unsigned n=in->size();  // input size
  const int arg0=MAX(lg(n+4095)-20, 0);  // block size

  // Get type from method "LB,R,t" where L is level 0..5, B is block
  // size 0..11, R is redundancy 0..255, t = 0..3 = binary, text, exe, both.
  unsigned type=0;
  if (isdigit(method[0])) {
    int commas=0, arg[4]={0};
    for (int i=1; i<int(method.size()) && commas<4; ++i) {
      if (method[i]==',' || method[i]=='.') ++commas;
      else if (isdigit(method[i])) arg[commas]=arg[commas]*10+method[i]-'0';
    }
    if (commas==0) type=512;
    else type=arg[1]*4+arg[2];
  }

  // Get hash of input
  libzpaq::SHA1 sha1;
  const char* sha1ptr=0;
  if (dosha1) {
    sha1.write(in->c_str(), n);
    sha1ptr=sha1.result();
  }

  // Expand default methods
  if (isdigit(method[0])) {
    const int level=method[0]-'0';

    // build models
    const int doe8=(type&2)*2;
    method="x"+itos(arg0);
    std::string htsz=","+itos(19+arg0+(arg0<=6));  // lz77 hash table size
    std::string sasz=","+itos(21+arg0);            // lz77 suffix array size

    // store uncompressed
    if (level==0)
      method="0"+itos(arg0)+",0";

    // LZ77, no model. Store if hard to compress
    else if (level==1) {
      if (type<40) method+=",0";
      else {
        method+=","+itos(1+doe8)+",";
        if      (type<80)  method+="4,0,1,15";
        else if (type<128) method+="4,0,2,16";
        else if (type<256) method+="4,0,2"+htsz;
        else if (type<960) method+="5,0,3"+htsz;
        else               method+="6,0,3"+htsz;
      }
    }

    // LZ77 with longer search
    else if (level==2) {
      if (type<32) method+=",0";
      else {
        method+=","+itos(1+doe8)+",";
        if (type<64) method+="4,0,3"+htsz;
        else method+="4,0,7"+sasz+",1";
      }
    }

    // LZ77 with CM depending on redundancy
    else if (level==3) {
      if (type<20)  // store if not compressible
        method+=",0";
      else if (type<48)  // fast LZ77 if barely compressible
        method+=","+itos(1+doe8)+",4,0,3"+htsz;
      else if (type>=640 || (type&1))  // BWT if text or highly compressible
        method+=","+itos(3+doe8)+"ci1";
      else  // LZ77 with O0-1 compression of up to 12 literals
        method+=","+itos(2+doe8)+",12,0,7"+sasz+",1c0,0,511i2";
    }

    // LZ77+CM, fast CM, or BWT depending on type
    else if (level==4) {
      if (type<12)
        method+=",0";
      else if (type<24)
        method+=","+itos(1+doe8)+",4,0,3"+htsz;
      else if (type<48)
        method+=","+itos(2+doe8)+",5,0,7"+sasz+"1c0,0,511";
      else if (type<900) {
        method+=","+itos(doe8)+"ci1,1,1,1,2a";
        if (type&1) method+="w";
        method+="m";
      }
      else
        method+=","+itos(3+doe8)+"ci1";
    }

    // Slow CM with lots of models
    else {  // 5..9

      // Model text files
      method+=","+itos(doe8);
      if (type&1) method+="w2c0,1010,255i1";
      else method+="w1i1";
      method+="c256ci1,1,1,1,1,1,2a";

      // Analyze the data
      const int NR=1<<12;
      int pt[256]={0};  // position of last occurrence
      int r[NR]={0};    // count repetition gaps of length r
      const unsigned char* p=in->data();
      if (level>0) {
        for (unsigned i=0; i<n; ++i) {
          const int k=i-pt[p[i]];
          if (k>0 && k<NR) ++r[k];
          pt[p[i]]=i;
        }
      }

      // Add periodic models
      int n1=n-r[1]-r[2]-r[3];
      for (int i=0; i<2; ++i) {
        int period=0;
        double score=0;
        int t=0;
        for (int j=5; j<NR && t<n1; ++j) {
          const double s=r[j]/(256.0+n1-t);
          if (s>score) score=s, period=j;
          t+=r[j];
        }
        if (period>4 && score>0.1) {
          method+="c0,0,"+itos(999+period)+",255i1";
          if (period<=255)
            method+="c0,"+itos(period)+"i1";
          n1-=r[period];
          r[period]=0;
        }
        else
          break;
      }
      method+="c0,2,0,255i1c0,3,0,0,255i1c0,4,0,0,0,255i1mm16ts19t0";
    }
  }

  // Compress
  std::string config;
  int args[9]={0};
  config=makeConfig(method.c_str(), args);
  libzpaq::Compressor co;
  co.setOutput(out);
  StringBuffer pcomp_cmd;
  co.writeTag();
  co.startBlock(config.c_str(), args, &pcomp_cmd);
  std::string cs=itos(n);
  if (comment) cs=cs+" "+comment;
  co.startSegment(filename, cs.c_str());
  if (args[1]>=1 && args[1]<=7 && args[1]!=4) {  // LZ77 or BWT
    LZBuffer lz(*in, args);
    co.setInput(&lz);
    co.compress();
  }
  else {  // compress with e8e9 or no preprocessing
    if (args[1]>=4 && args[1]<=7)
      e8e9(in->data(), in->size());
    co.setInput(in);
    co.compress();
  }
  co.endSegment(sha1ptr);
  co.endBlock();
}

}  // end namespace libzpaq

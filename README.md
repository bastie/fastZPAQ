# ZPAQ

zpaq is a journaling archiver optimized for user-level incremental
backup of directory trees in MacOS/X. It supports AES-256
encryption, 5 multi-threaded compression levels, and content-aware
file fragment level deduplication. For backups it adds only files
whose date has changed, and keeps both old and new versions. You can roll
back the archive date to restore from old versions of the archive.
The default compression level is faster than zip usually with better
compression. zpaq uses a self-describing compressed format to allow
for future improvements without breaking compatibility with older
versions of the program. Contents:


    File            Ver.   Description
    -----------     ----   -----------
    zpaq.cpp        7.15   zpaq source code.
    zpaq.pod        7.12   zpaq man page in pod2man format.
    libzpaq.h       7.12   libzpaq API documentation and header.
    libzpaq.cpp     7.15   libzpaq API source code.
    Makefile               to compile with make {install|check|clean}.
    COPYING                Unlicense.

Original versions of this software can be found at
http://mattmahoney.net/dc/zpaq.html

Code from divsufsort.c is embedded in libzpaq.cpp.
divsufsort.c is (C) 2003-2008 Yuta Mori, MIT license (see source code).
It is also available from libdivsufsort-lite 2.0 from
http://code.google.com/p/libdivsufsort/
All remaining code is public domain. See COPYING.

zpaq is a command line program. For a brief description of the commands,
type "zpaq" with no arguments. See zpaq.pod for details.

## Compile

You can use "make" to compile for Mac OS/X or compile like this:

  ```g++ -O3 -march=apple-m1 zpaq.cpp libzpaq.cpp -o zpaq```

## Documentation

To generate a man page in Linux or Cygwin:

  ```pod2man zpaq.pod > zpaq.man```


## Install

```make install```


# fastZPAQ

## compare

Call with...

* xz : ```xz --extreme --threads=12 --keep [file]```
* zpaq (homebrew) : ```/opt/homebrew/Cellar/zpaq/7.15_1/bin/zpaq a [file].zpaq [file] -method 9``` (12 threads on my test system)
* zpaq (fast) : ```./zpaq a [file].zpaq [file] -method 9``` (self build, ASM specific)
* Infozip 3.0 : ```zip -9 [file].zip [file]``` (OS included)
* zstd : ```zstd --ultra -22 --long -T12 [file] -o [file].zstd```
* 7zip : ```7zz a -mmt12 -mx9 -slp [file].7z [file]```
* brotli : ```brotli -o [file].br -q 11 -w 24 [file]```
* Apple gzip 400 : ```gzip --best --keep [file]``` (OS included)
* zopfli : ```zopfli --i100 [file]```
* bzip2 : ```bzip2 -zk9 [enwik9]```
* pigz (gzip) : ```pigz --best --keep [file]```  (8 threads per default)
* pigz (zopfli) : ```pigz -11 -iterations 100 --keep [file]``` (8 threads per default)

get results like ...

    compressor       file         time                 size                         performance
    -------------------------------------------------------------------------------------------------
    none            enwik9           0 s      1.000.000.000 byte (100,00%)             all
    pigz (gzip)**   enwik9        5,21 s        322.914.671 byte ( 32,29%)         129.958.796 byte/s
    gzip            enwik9       30,66 s        322.789.249 byte ( 32,28%)          22.087.760 byte/s
    zip             enwik9       30,74 s        322.592.132 byte ( 32,26%)          22.036.690 byte/s
    bzip2           enwik9       48,87 s        253.977.891 byte ( 25,40%)          15.265.441 byte/s
    xz              enwik9       55,36 s        232.378.008 byte ( 23,24%)          13.866.004 byte/s
    7zip            enwik9      143,90 s        214.790.773 byte ( 21,48%)           5.456.631 byte/s
    zpaq (fast)     enwik9      298,79 s        168.590.741 byte ( 16,86%)           2.782.587 byte/s 
    zstd            enwik9      313,13 s        214.871.235 byte ( 21,49%)           2.507.337 byte/s
    zpaq (homebrew) enwik9      420,97 s        168.590.741 byte ( 16,86%)           1.974.984 byte/s
    pigz (zopfli)*  enwik9      986,69 s        309.510.749 byte ( 30,95%)             699.803 byte/s
    brotli          enwik9    1.418,68 s        223.348.686 byte ( 22,34%)             547.446 byte/s
    zopfli          enwik9    6.660,01 s        309.453.089 byte ( 30,95%)             103.685 byte/s
    
    *  in result of also time 800,07 s is reachable the real performance is     863.036 byte/s
    ** in result of also time   3,40 s is reachable the real performance is 199.142.743 byte/s

## cloc

In result of specific target Apple ARM64 (M1/M2) removed some unused code.

    -------------------------------------------------------------------------------
    Language                     files          blank        comment           code
    -------------------------------------------------------------------------------
    C++                              2            828           1179           9057
    C/C++ Header                     1            247            772            491
    Markdown                         1             31              0             87
    make                             1              8              0             28
    -------------------------------------------------------------------------------
    SUM:                             5           1114           1951           9663
    -------------------------------------------------------------------------------
    
** EOF **

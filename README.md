# ZPAQ

zpaq is a journaling archiver optimized for user-level incremental
backup of directory trees in Windows, MacOS/X and \*nix. It supports AES-256
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
Please report bugs to Matt Mahoney at mattmahoneyfl@gmail.com

Code from divsufsort.c is embedded in libzpaq.cpp.
divsufsort.c is (C) 2003-2008 Yuta Mori, MIT license (see source code).
It is also available from libdivsufsort-lite 2.0 from
http://code.google.com/p/libdivsufsort/
All remaining code is public domain. See COPYING.

zpaq is a command line program. For a brief description of the commands,
type "zpaq" with no arguments. See zpaq.pod for details.

## Compile

You can use "make" to compile for Mac OS/X or compile like this:

  ```g++ -O3 -march=apple-m1 -DNOJIT -Dunix zpaq.cpp libzpaq.cpp -pthread -o zpaq```

To compile for non x86 or x86-64 hardware use option -DNOJIT

Options have the following meanings:

    -Dunix   = select Unix or Linux target in zpaq and libzpaq.
    -DDEBUG  = turn on run time checks.
    -DNOJIT  = turn off run time optimization of ZPAQL to 32 or 64 bit x86
               in libzpaq. Use this for a ARM processor.
    -pthread = link to pthread library (required in unix/Linux).

General options:

    -O3      = optimize for speed.
    -s       = strip debugging symbols. (Some compilers ignore this).
    -static  = use this if you plan to run the program on a different
                   machine than you compiled it on. Makes the executable bigger.

## Documentation

To generate a man page in Linux or Cygwin:

  ```pod2man zpaq.pod > zpaq.man```


## Install

```make install```


# fastZPAQ

## compare

Call with...

* xz : ```xz --extreme --threads=12 --keep [file]```
* zpaq (homebrew) : ```/opt/homebrew/Cellar/zpaq/7.15_1/bin/zpaq a [file].zpaq [file] -threads 12 -method 9```
* zpaq (fast) : ```./zpaq a [file].zpaq [file] -threads 12 -method 9``` (self build)
* Infozip 3.0 : ```zip -9 [file].zip [file]``` (OS included)
* zstd : ```zstd --ultra -22 --long -T12 [file] -o [file].zstd```
* 7zip : ```7zz a -mmt12 -mx9 -slp [file].7z [file]```
* brotli : ```brotli -o [file].br -q 11 -w 24 [file]```
* Apple gzip 400 : ```gzip --best --keep [file]``` (OS included)
* zopfli : ```zopfli --i100 [file]```

get results like ...

    compressor       file         time                 size                         performance
    -------------------------------------------------------------------------------------------------
    none            enwik9           0 s      1.000.000.000 byte (100,00%)             all
    gzip            enwik9       30,66 s        322.789.249 byte ( 32,28%)          22.087.760 byte/s
    zip             enwik9       30,74 s        322.592.132 byte ( 32,26%)          22.036.690 byte/s
    xz              enwik9       55,36 s        232.378.008 byte ( 23,24%)          13.866.004 byte/s
    7zip            enwik9      143,90 s        214.790.773 byte ( 21,48%)           5.456.631 byte/s
    zstd            enwik9      313,13 s        214.877.283 byte ( 21,49%)           2.507.337 byte/s
    zpag (fast)     enwik9      410,51 s        168.590.741 byte ( 16,86%)           2.025.308 byte/s
    zpaq (homebrew) enwik9      420,97 s        168.590.741 byte ( 16,86%)           1.974.984 byte/s
    brotli          enwik9    1.418,68 s        223.348.686 byte ( 22,34%)             547.446 byte/s
    zopfli          enwik9    6.660,01 s        309.453.089 byte ( 30,95%)             103.685 byte/s

## cloc

count line of codes and comments

    -------------------------------------------------------------------------------
    Language                     files          blank        comment           code
    -------------------------------------------------------------------------------
    C++                              2            857           1195           9418
    C/C++ Header                     1            247            772            491
    Markdown                         1             27              0             74
    make                             1              8              0             28
    -------------------------------------------------------------------------------
    SUM:                             5           1139           1967          10011
    -------------------------------------------------------------------------------

** EOF **

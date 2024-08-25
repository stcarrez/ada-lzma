# Ada LZMA Library Binding

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/lzmada.json)](https://alire.ada.dev/crates/lzmada)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-lzma/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-lzma/summary)
[![License](https://img.shields.io/:license-mit-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-lzma)
[![Download](https://img.shields.io/badge/download-1.1.3-brightgreen.svg)](http://download.vacs.fr/ada-lzma/ada-lzma-1.1.3.tar.gz)

This is a small Ada05 library binding for the liblzma library.

## Version 1.1.4   - Aug 2024
  - Cleanup build environment to drop configure

[List all versions](https://gitlab.com/stcarrez/ada-lzma/blob/master/NEWS.md)

## About liblzma

liblzma is a public domain general-purpose data compression library with
a zlib-like API.
 
liblzma is part of XZ Utils <https://tukaani.org/xz/>. XZ Utils includes
a gzip-like command line tool named xz and some other tools. XZ Utils
is developed and maintained by Lasse Collin.

Major parts of liblzma are based on Igor Pavlov's public domain LZMA SDK
<https://7-zip.org/sdk.html>.

## Building ada-lzma

Before you build the package you must install the `liblzma` library.
The installation of `liblzma` depends on your system:

| System           | Install with
|------------------|---------------------------
| Ubuntu & Debian  | `sudo apt-get install liblzma-dev`
| Fedora           | `sudo yum install xz-devel`
| NetBSD           | *pre installed (contrib/xz)*
| FreeBSD          | *pre installed (contrib/xz)*
| Windows          | See https://tukaani.org/xz/
| Source           | See https://tukaani.org/xz/

## Build with Alire

To use lzmada in your project, use the following command:

```
alr with lzmada
```

Build with the following commands:

    make

## Installation

The installation is done with `gprinstall` if the configure script found it or
by traditional copy.  In all cases, you should install with the next command:

    make install

## How to use Ada LZMA

Two examples are provided to illustrate how to use the library.
Look at these examples for the details on how to use some of the operations.

samples/compress_easy.adb is an example on how to compress some stream.
samples/decompress.adb shows hoz to decompress a stream.

Roughly speaking, import some package:

    with Lzma.Base;
    with Lzma.Container;
    with Lzma.Check;

Then declare the LZMA stream:

    Stream  : aliased Lzma.Base.lzma_stream := Lzma.Base.LZMA_STREAM_INIT;

Initialize the LZMA stream as decoder (or as encoder):

    Result := Lzma.Container.lzma_stream_decoder (Stream'Unchecked_Access,
                                                  Long_Long_Integer'Last,
                                                  Lzma.Container.LZMA_CONCATENATED);

Setup the stream 'next_out', 'avail_out', 'next_in' and 'avail_in' and call
the lzma_code operation with the action (Lzma.Base.LZMA_RUN or Lzma.Base.LZMA_FINISH):

    Result := Lzma.Base.lzma_code (Stream'Unchecked_Access, Action);

Close the LZMA stream:

    Lzma.Base.lzma_end (Stream'Unchecked_Access);


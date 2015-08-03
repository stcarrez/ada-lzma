Ada LZMA Library Binding

This is a small Ada05 library binding for the liblzma library.

## About liblzma

liblzma is a public domain general-purpose data compression library with
a zlib-like API.
 
liblzma is part of XZ Utils <http://tukaani.org/xz/>. XZ Utils includes
a gzip-like command line tool named xz and some other tools. XZ Utils
is developed and maintained by Lasse Collin.

Major parts of liblzma are based on Igor Pavlov's public domain LZMA SDK
<http://7-zip.org/sdk.html>.

## Building ada-lzma

Build with the following commands:

  ./configure
  make

## Installation

  make install

## How to use Ada LZMA

Two examples are provided to illustrate how to use the library.
Look at these examples for the details on how to use some of the operations.

samples/compress_easy.adb is an example on how to compress some stream.
samples/decompress.adb shows hoz to decompress a stream.

Roughtfly speaking, import some package:

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

#!/bin/sh
# This script is used to generate the Ada binding from the LZMA header file.
# Some transformations are made to get a better Ada style for the generated binding.
# - the stddef_h.ads and stdint_h.ads Ada binding files are not used.
# - the package layout is changed to Lzma.Base, Lzma.Check, Lzma.Container, ...
DIR=`dirname $0`
TMP=/tmp/mk-lzma
mkdir -p $TMP
(cd $TMP && g++ -fdump-ada-spec -C /usr/include/lzma.h)
sed -f $DIR/lzma.sed < $TMP/lzma_base_h.ads > lzma-base.ads
sed -f $DIR/lzma.sed < $TMP/lzma_check_h.ads > lzma-check.ads
sed -f $DIR/lzma.sed < $TMP/lzma_block_h.ads > lzma-block.ads
sed -f $DIR/lzma.sed < $TMP/lzma_filter_h.ads > lzma-filter.ads
sed -f $DIR/lzma.sed < $TMP/lzma_vli_h.ads > lzma-vli.ads
sed -f $DIR/lzma.sed < $TMP/lzma_container_h.ads > lzma-container.ads
sed -f $DIR/lzma.sed < $TMP/lzma_lzma_h.ads > lzma_lzma_h.ads
sed -f $DIR/lzma.sed < $TMP/lzma_stream_flags_h.ads > lzma-streamflags.ads


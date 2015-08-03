pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Ada.Streams;
with Lzma.Base;
with Lzma.Check;
limited with Lzma.Filter;
limited with lzma_lzma_h;

package Lzma.Container is

   LZMA_PRESET_DEFAULT : constant Interfaces.C.unsigned := 6;
   LZMA_PRESET_LEVEL_MASK : constant Interfaces.C.unsigned := 16#1F#;
   LZMA_PRESET_EXTREME : constant Interfaces.C.unsigned := 16#80000000#;
   LZMA_TELL_NO_CHECK : constant Interfaces.C.unsigned := 1;
   LZMA_TELL_UNSUPPORTED_CHECK : constant Interfaces.C.unsigned := 2;
   LZMA_TELL_ANY_CHECK : constant Interfaces.C.unsigned := 4;
   LZMA_CONCATENATED : constant Interfaces.C.unsigned := 8;
  --*
  -- * \file        lzma/container.h
  -- * \brief       File formats
  --

  -- * Author: Lasse Collin
  -- *
  -- * This file has been put into the public domain.
  -- * You can do whatever you want with this file.
  -- *
  -- * See ../lzma.h for information about liblzma as a whole.
  --

  --***********
  -- * Encoding *
  -- ***********

  --*
  -- * \brief       Default compression preset
  -- *
  -- * It's not straightforward to recommend a default preset, because in some
  -- * cases keeping the resource usage relatively low is more important that
  -- * getting the maximum compression ratio.
  --

  --*
  -- * \brief       Mask for preset level
  -- *
  -- * This is useful only if you need to extract the level from the preset
  -- * variable. That should be rare.
  --

  -- * Preset flags
  -- *
  -- * Currently only one flag is defined.
  --

  --*
  -- * \brief       Extreme compression preset
  -- *
  -- * This flag modifies the preset to make the encoding significantly slower
  -- * while improving the compression ratio only marginally. This is useful
  -- * when you don't mind wasting time to get as small result as possible.
  -- *
  -- * This flag doesn't affect the memory usage requirements of the decoder (at
  -- * least not significantly). The memory usage of the encoder may be increased
  -- * a little but only at the lowest preset levels (0-3).
  --

  --*
  -- * \brief       Calculate approximate memory usage of easy encoder
  -- *
  -- * This function is a wrapper for lzma_raw_encoder_memusage().
  -- *
  -- * \param       preset  Compression preset (level and possible flags)
  -- *
  -- * \return      Number of bytes of memory required for the given
  -- *              preset when encoding. If an error occurs, for example
  -- *              due to unsupported preset, UINT64_MAX is returned.
  --

   function lzma_easy_encoder_memusage (preset : Interfaces.C.unsigned) return Long_Long_Integer;  -- /usr/include/lzma/container.h:74
   pragma Import (C, lzma_easy_encoder_memusage, "lzma_easy_encoder_memusage");

  --*
  -- * \brief       Calculate approximate decoder memory usage of a preset
  -- *
  -- * This function is a wrapper for lzma_raw_decoder_memusage().
  -- *
  -- * \param       preset  Compression preset (level and possible flags)
  -- *
  -- * \return      Number of bytes of memory required to decompress a file
  -- *              that was compressed using the given preset. If an error
  -- *              occurs, for example due to unsupported preset, UINT64_MAX
  -- *              is returned.
  --

   function lzma_easy_decoder_memusage (preset : Interfaces.C.unsigned) return Long_Long_Integer;  -- /usr/include/lzma/container.h:90
   pragma Import (C, lzma_easy_decoder_memusage, "lzma_easy_decoder_memusage");

  --*
  -- * \brief       Initialize .xz Stream encoder using a preset number
  -- *
  -- * This function is intended for those who just want to use the basic features
  -- * if liblzma (that is, most developers out there).
  -- *
  -- * \param       strm    Pointer to lzma_stream that is at least initialized
  -- *                      with LZMA_STREAM_INIT.
  -- * \param       preset  Compression preset to use. A preset consist of level
  -- *                      number and zero or more flags. Usually flags aren't
  -- *                      used, so preset is simply a number [0, 9] which match
  -- *                      the options -0 ... -9 of the xz command line tool.
  -- *                      Additional flags can be be set using bitwise-or with
  -- *                      the preset level number, e.g. 6 | LZMA_PRESET_EXTREME.
  -- * \param       check   Integrity check type to use. See check.h for available
  -- *                      checks. The xz command line tool defaults to
  -- *                      LZMA_CHECK_CRC64, which is a good choice if you are
  -- *                      unsure. LZMA_CHECK_CRC32 is good too as long as the
  -- *                      uncompressed file is not many gigabytes.
  -- *
  -- * \return      - LZMA_OK: Initialization succeeded. Use lzma_code() to
  -- *                encode your data.
  -- *              - LZMA_MEM_ERROR: Memory allocation failed.
  -- *              - LZMA_OPTIONS_ERROR: The given compression preset is not
  -- *                supported by this build of liblzma.
  -- *              - LZMA_UNSUPPORTED_CHECK: The given check type is not
  -- *                supported by this liblzma build.
  -- *              - LZMA_PROG_ERROR: One or more of the parameters have values
  -- *                that will never be valid. For example, strm == NULL.
  -- *
  -- * If initialization fails (return value is not LZMA_OK), all the memory
  -- * allocated for *strm by liblzma is always freed. Thus, there is no need
  -- * to call lzma_end() after failed initialization.
  -- *
  -- * If initialization succeeds, use lzma_code() to do the actual encoding.
  -- * Valid values for `action' (the second argument of lzma_code()) are
  -- * LZMA_RUN, LZMA_SYNC_FLUSH, LZMA_FULL_FLUSH, and LZMA_FINISH. In future,
  -- * there may be compression levels or flags that don't support LZMA_SYNC_FLUSH.
  --

   function lzma_easy_encoder
     (strm : access Lzma.Base.lzma_stream;
      preset : Interfaces.C.unsigned;
      check : Lzma.Check.lzma_check) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:133
   pragma Import (C, lzma_easy_encoder, "lzma_easy_encoder");

  --*
  -- * \brief       Single-call .xz Stream encoding using a preset number
  -- *
  -- * The maximum required output buffer size can be calculated with
  -- * lzma_stream_buffer_bound().
  -- *
  -- * \param       preset      Compression preset to use. See the description
  -- *                          in lzma_easy_encoder().
  -- * \param       check       Type of the integrity check to calculate from
  -- *                          uncompressed data.
  -- * \param       allocator   lzma_allocator for custom allocator functions.
  -- *                          Set to NULL to use malloc() and free().
  -- * \param       in          Beginning of the input buffer
  -- * \param       in_size     Size of the input buffer
  -- * \param       out         Beginning of the output buffer
  -- * \param       out_pos     The next byte will be written to out[*out_pos].
  -- *                          *out_pos is updated only if encoding succeeds.
  -- * \param       out_size    Size of the out buffer; the first byte into
  -- *                          which no data is written to is out[out_size].
  -- *
  -- * \return      - LZMA_OK: Encoding was successful.
  -- *              - LZMA_BUF_ERROR: Not enough output buffer space.
  -- *              - LZMA_UNSUPPORTED_CHECK
  -- *              - LZMA_OPTIONS_ERROR
  -- *              - LZMA_MEM_ERROR
  -- *              - LZMA_DATA_ERROR
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_easy_buffer_encode
     (preset : Interfaces.C.unsigned;
      check : Lzma.Check.lzma_check;
      allocator : access Lzma.Base.lzma_allocator;
      c_in : access Ada.Streams.Stream_Element;
      in_size : Interfaces.C.size_t;
      c_out : access Ada.Streams.Stream_Element;
      out_pos : access Interfaces.C.size_t;
      out_size : Interfaces.C.size_t) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:166
   pragma Import (C, lzma_easy_buffer_encode, "lzma_easy_buffer_encode");

  --*
  -- * \brief       Initialize .xz Stream encoder using a custom filter chain
  -- *
  -- * \param       strm    Pointer to properly prepared lzma_stream
  -- * \param       filters Array of filters. This must be terminated with
  -- *                      filters[n].id = LZMA_VLI_UNKNOWN. See filter.h for
  -- *                      more information.
  -- * \param       check   Type of the integrity check to calculate from
  -- *                      uncompressed data.
  -- *
  -- * \return      - LZMA_OK: Initialization was successful.
  -- *              - LZMA_MEM_ERROR
  -- *              - LZMA_UNSUPPORTED_CHECK
  -- *              - LZMA_OPTIONS_ERROR
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_stream_encoder
     (strm : access Lzma.Base.lzma_stream;
      filters : access constant Lzma.Filter.lzma_filter;
      check : Lzma.Check.lzma_check) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:188
   pragma Import (C, lzma_stream_encoder, "lzma_stream_encoder");

  --*
  -- * \brief       Initialize .lzma encoder (legacy file format)
  -- *
  -- * The .lzma format is sometimes called the LZMA_Alone format, which is the
  -- * reason for the name of this function. The .lzma format supports only the
  -- * LZMA1 filter. There is no support for integrity checks like CRC32.
  -- *
  -- * Use this function if and only if you need to create files readable by
  -- * legacy LZMA tools such as LZMA Utils 4.32.x. Moving to the .xz format
  -- * is strongly recommended.
  -- *
  -- * The valid action values for lzma_code() are LZMA_RUN and LZMA_FINISH.
  -- * No kind of flushing is supported, because the file format doesn't make
  -- * it possible.
  -- *
  -- * \return      - LZMA_OK
  -- *              - LZMA_MEM_ERROR
  -- *              - LZMA_OPTIONS_ERROR
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_alone_encoder (strm : access Lzma.Base.lzma_stream; options : access constant lzma_lzma_h.lzma_options_lzma) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:213
   pragma Import (C, lzma_alone_encoder, "lzma_alone_encoder");

  --*
  -- * \brief       Calculate output buffer size for single-call Stream encoder
  -- *
  -- * When trying to compress uncompressible data, the encoded size will be
  -- * slightly bigger than the input data. This function calculates how much
  -- * output buffer space is required to be sure that lzma_stream_buffer_encode()
  -- * doesn't return LZMA_BUF_ERROR.
  -- *
  -- * The calculated value is not exact, but it is guaranteed to be big enough.
  -- * The actual maximum output space required may be slightly smaller (up to
  -- * about 100 bytes). This should not be a problem in practice.
  -- *
  -- * If the calculated maximum size doesn't fit into size_t or would make the
  -- * Stream grow past LZMA_VLI_MAX (which should never happen in practice),
  -- * zero is returned to indicate the error.
  -- *
  -- * \note        The limit calculated by this function applies only to
  -- *              single-call encoding. Multi-call encoding may (and probably
  -- *              will) have larger maximum expansion when encoding
  -- *              uncompressible data. Currently there is no function to
  -- *              calculate the maximum expansion of multi-call encoding.
  --

   function lzma_stream_buffer_bound (uncompressed_size : Interfaces.C.size_t) return Interfaces.C.size_t;  -- /usr/include/lzma/container.h:240
   pragma Import (C, lzma_stream_buffer_bound, "lzma_stream_buffer_bound");

  --*
  -- * \brief       Single-call .xz Stream encoder
  -- *
  -- * \param       filters     Array of filters. This must be terminated with
  -- *                          filters[n].id = LZMA_VLI_UNKNOWN. See filter.h
  -- *                          for more information.
  -- * \param       check       Type of the integrity check to calculate from
  -- *                          uncompressed data.
  -- * \param       allocator   lzma_allocator for custom allocator functions.
  -- *                          Set to NULL to use malloc() and free().
  -- * \param       in          Beginning of the input buffer
  -- * \param       in_size     Size of the input buffer
  -- * \param       out         Beginning of the output buffer
  -- * \param       out_pos     The next byte will be written to out[*out_pos].
  -- *                          *out_pos is updated only if encoding succeeds.
  -- * \param       out_size    Size of the out buffer; the first byte into
  -- *                          which no data is written to is out[out_size].
  -- *
  -- * \return      - LZMA_OK: Encoding was successful.
  -- *              - LZMA_BUF_ERROR: Not enough output buffer space.
  -- *              - LZMA_UNSUPPORTED_CHECK
  -- *              - LZMA_OPTIONS_ERROR
  -- *              - LZMA_MEM_ERROR
  -- *              - LZMA_DATA_ERROR
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_stream_buffer_encode
     (filters : access Lzma.Filter.lzma_filter;
      check : Lzma.Check.lzma_check;
      allocator : access Lzma.Base.lzma_allocator;
      c_in : access Ada.Streams.Stream_Element;
      in_size : Interfaces.C.size_t;
      c_out : access Ada.Streams.Stream_Element;
      out_pos : access Interfaces.C.size_t;
      out_size : Interfaces.C.size_t) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:270
   pragma Import (C, lzma_stream_buffer_encode, "lzma_stream_buffer_encode");

  --***********
  -- * Decoding *
  -- ***********

  --*
  -- * This flag makes lzma_code() return LZMA_NO_CHECK if the input stream
  -- * being decoded has no integrity check. Note that when used with
  -- * lzma_auto_decoder(), all .lzma files will trigger LZMA_NO_CHECK
  -- * if LZMA_TELL_NO_CHECK is used.
  --

  --*
  -- * This flag makes lzma_code() return LZMA_UNSUPPORTED_CHECK if the input
  -- * stream has an integrity check, but the type of the integrity check is not
  -- * supported by this liblzma version or build. Such files can still be
  -- * decoded, but the integrity check cannot be verified.
  --

  --*
  -- * This flag makes lzma_code() return LZMA_GET_CHECK as soon as the type
  -- * of the integrity check is known. The type can then be got with
  -- * lzma_get_check().
  --

  --*
  -- * This flag enables decoding of concatenated files with file formats that
  -- * allow concatenating compressed files as is. From the formats currently
  -- * supported by liblzma, only the .xz format allows concatenated files.
  -- * Concatenated files are not allowed with the legacy .lzma format.
  -- *
  -- * This flag also affects the usage of the `action' argument for lzma_code().
  -- * When LZMA_CONCATENATED is used, lzma_code() won't return LZMA_STREAM_END
  -- * unless LZMA_FINISH is used as `action'. Thus, the application has to set
  -- * LZMA_FINISH in the same way as it does when encoding.
  -- *
  -- * If LZMA_CONCATENATED is not used, the decoders still accept LZMA_FINISH
  -- * as `action' for lzma_code(), but the usage of LZMA_FINISH isn't required.
  --

  --*
  -- * \brief       Initialize .xz Stream decoder
  -- *
  -- * \param       strm        Pointer to properly prepared lzma_stream
  -- * \param       memlimit    Memory usage limit as bytes. Use UINT64_MAX
  -- *                          to effectively disable the limiter.
  -- * \param       flags       Bitwise-or of zero or more of the decoder flags:
  -- *                          LZMA_TELL_NO_CHECK, LZMA_TELL_UNSUPPORTED_CHECK,
  -- *                          LZMA_TELL_ANY_CHECK, LZMA_CONCATENATED
  -- *
  -- * \return      - LZMA_OK: Initialization was successful.
  -- *              - LZMA_MEM_ERROR: Cannot allocate memory.
  -- *              - LZMA_OPTIONS_ERROR: Unsupported flags
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_stream_decoder
     (strm : access Lzma.Base.lzma_stream;
      memlimit : Long_Long_Integer;
      flags : Interfaces.C.unsigned) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:339
   pragma Import (C, lzma_stream_decoder, "lzma_stream_decoder");

  --*
  -- * \brief       Decode .xz Streams and .lzma files with autodetection
  -- *
  -- * This decoder autodetects between the .xz and .lzma file formats, and
  -- * calls lzma_stream_decoder() or lzma_alone_decoder() once the type
  -- * of the input file has been detected.
  -- *
  -- * \param       strm        Pointer to properly prepared lzma_stream
  -- * \param       memlimit    Memory usage limit as bytes. Use UINT64_MAX
  -- *                          to effectively disable the limiter.
  -- * \param       flags       Bitwise-or of flags, or zero for no flags.
  -- *
  -- * \return      - LZMA_OK: Initialization was successful.
  -- *              - LZMA_MEM_ERROR: Cannot allocate memory.
  -- *              - LZMA_OPTIONS_ERROR: Unsupported flags
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_auto_decoder
     (strm : access Lzma.Base.lzma_stream;
      memlimit : Long_Long_Integer;
      flags : Interfaces.C.unsigned) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:361
   pragma Import (C, lzma_auto_decoder, "lzma_auto_decoder");

  --*
  -- * \brief       Initialize .lzma decoder (legacy file format)
  -- *
  -- * Valid `action' arguments to lzma_code() are LZMA_RUN and LZMA_FINISH.
  -- * There is no need to use LZMA_FINISH, but allowing it may simplify
  -- * certain types of applications.
  -- *
  -- * \return      - LZMA_OK
  -- *              - LZMA_MEM_ERROR
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_alone_decoder (strm : access Lzma.Base.lzma_stream; memlimit : Long_Long_Integer) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:377
   pragma Import (C, lzma_alone_decoder, "lzma_alone_decoder");

  --*
  -- * \brief       Single-call .xz Stream decoder
  -- *
  -- * \param       memlimit    Pointer to how much memory the decoder is allowed
  -- *                          to allocate. The value pointed by this pointer is
  -- *                          modified if and only if LZMA_MEMLIMIT_ERROR is
  -- *                          returned.
  -- * \param       flags       Bitwise-or of zero or more of the decoder flags:
  -- *                          LZMA_TELL_NO_CHECK, LZMA_TELL_UNSUPPORTED_CHECK,
  -- *                          LZMA_CONCATENATED. Note that LZMA_TELL_ANY_CHECK
  -- *                          is not allowed and will return LZMA_PROG_ERROR.
  -- * \param       allocator   lzma_allocator for custom allocator functions.
  -- *                          Set to NULL to use malloc() and free().
  -- * \param       in          Beginning of the input buffer
  -- * \param       in_pos      The next byte will be read from in[*in_pos].
  -- *                          *in_pos is updated only if decoding succeeds.
  -- * \param       in_size     Size of the input buffer; the first byte that
  -- *                          won't be read is in[in_size].
  -- * \param       out         Beginning of the output buffer
  -- * \param       out_pos     The next byte will be written to out[*out_pos].
  -- *                          *out_pos is updated only if decoding succeeds.
  -- * \param       out_size    Size of the out buffer; the first byte into
  -- *                          which no data is written to is out[out_size].
  -- *
  -- * \return      - LZMA_OK: Decoding was successful.
  -- *              - LZMA_FORMAT_ERROR
  -- *              - LZMA_OPTIONS_ERROR
  -- *              - LZMA_DATA_ERROR
  -- *              - LZMA_NO_CHECK: This can be returned only if using
  -- *                the LZMA_TELL_NO_CHECK flag.
  -- *              - LZMA_UNSUPPORTED_CHECK: This can be returned only if using
  -- *                the LZMA_TELL_UNSUPPORTED_CHECK flag.
  -- *              - LZMA_MEM_ERROR
  -- *              - LZMA_MEMLIMIT_ERROR: Memory usage limit was reached.
  -- *                The minimum required memlimit value was stored to *memlimit.
  -- *              - LZMA_BUF_ERROR: Output buffer was too small.
  -- *              - LZMA_PROG_ERROR
  --

   function lzma_stream_buffer_decode
     (memlimit : access Long_Long_Integer;
      flags : Interfaces.C.unsigned;
      allocator : access Lzma.Base.lzma_allocator;
      c_in : access Ada.Streams.Stream_Element;
      in_pos : access Interfaces.C.size_t;
      in_size : Interfaces.C.size_t;
      c_out : access Ada.Streams.Stream_Element;
      out_pos : access Interfaces.C.size_t;
      out_size : Interfaces.C.size_t) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/container.h:420
   pragma Import (C, lzma_stream_buffer_decode, "lzma_stream_buffer_decode");

end Lzma.Container;

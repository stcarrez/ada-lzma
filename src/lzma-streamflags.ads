pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Ada.Streams;
with Lzma.Vli;
with Lzma.Check;
with Lzma.Base;

package Lzma.StreamFlags is


   LZMA_STREAM_HEADER_SIZE : constant := 12;  --  /usr/include/lzma/stream_flags.h:27

   LZMA_BACKWARD_SIZE_MIN : constant := 4;  --  /usr/include/lzma/stream_flags.h:70
   --  unsupported macro: LZMA_BACKWARD_SIZE_MAX (LZMA_VLI_C(1) << 34)

  --*
  -- * \file        lzma/stream_flags.h
  -- * \brief       .xz Stream Header and Stream Footer encoder and decoder
  --

  -- * Author: Lasse Collin
  -- *
  -- * This file has been put into the public domain.
  -- * You can do whatever you want with this file.
  -- *
  -- * See ../lzma.h for information about liblzma as a whole.
  --

  --*
  -- * \brief       Size of Stream Header and Stream Footer
  -- *
  -- * Stream Header and Stream Footer have the same size and they are not
  -- * going to change even if a newer version of the .xz file format is
  -- * developed in future.
  --

  --*
  -- * \brief       Options for encoding/decoding Stream Header and Stream Footer
  --

  --*
  --  * \brief       Stream Flags format version
  --  *
  --  * To prevent API and ABI breakages if new features are needed in
  --  * Stream Header or Stream Footer, a version number is used to
  --  * indicate which fields in this structure are in use. For now,
  --  * version must always be zero. With non-zero version, the
  --  * lzma_stream_header_encode() and lzma_stream_footer_encode()
  --  * will return LZMA_OPTIONS_ERROR.
  --  *
  --  * lzma_stream_header_decode() and lzma_stream_footer_decode()
  --  * will always set this to the lowest value that supports all the
  --  * features indicated by the Stream Flags field. The application
  --  * must check that the version number set by the decoding functions
  --  * is supported by the application. Otherwise it is possible that
  --  * the application will decode the Stream incorrectly.
  --

   type lzma_stream_flags is record
      version : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/stream_flags.h:51
      backward_size : aliased Lzma.Vli.lzma_vli;  -- /usr/include/lzma/stream_flags.h:69
      check : aliased Lzma.Check.lzma_check;  -- /usr/include/lzma/stream_flags.h:79
      reserved_enum1 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/stream_flags.h:90
      reserved_enum2 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/stream_flags.h:91
      reserved_enum3 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/stream_flags.h:92
      reserved_enum4 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/stream_flags.h:93
      reserved_bool1 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:94
      reserved_bool2 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:95
      reserved_bool3 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:96
      reserved_bool4 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:97
      reserved_bool5 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:98
      reserved_bool6 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:99
      reserved_bool7 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:100
      reserved_bool8 : aliased Lzma.Base.lzma_bool;  -- /usr/include/lzma/stream_flags.h:101
      reserved_int1 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/stream_flags.h:102
      reserved_int2 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/stream_flags.h:103
   end record;
   pragma Convention (C_Pass_By_Copy, lzma_stream_flags);  -- /usr/include/lzma/stream_flags.h:105

   --  skipped anonymous struct anon_14

  --*
  --  * \brief       Backward Size
  --  *
  --  * Backward Size must be a multiple of four bytes. In this Stream
  --  * format version, Backward Size is the size of the Index field.
  --  *
  --  * Backward Size isn't actually part of the Stream Flags field, but
  --  * it is convenient to include in this structure anyway. Backward
  --  * Size is present only in the Stream Footer. There is no need to
  --  * initialize backward_size when encoding Stream Header.
  --  *
  --  * lzma_stream_header_decode() always sets backward_size to
  --  * LZMA_VLI_UNKNOWN so that it is convenient to use
  --  * lzma_stream_flags_compare() when both Stream Header and Stream
  --  * Footer have been decoded.
  --

  --*
  --  * \brief       Check ID
  --  *
  --  * This indicates the type of the integrity check calculated from
  --  * uncompressed data.
  --

  --  * Reserved space to allow possible future extensions without
  --  * breaking the ABI. You should not touch these, because the
  --  * names of these variables may change.
  --  *
  --  * (We will never be able to use all of these since Stream Flags
  --  * is just two bytes plus Backward Size of four bytes. But it's
  --  * nice to have the proper types when they are needed.)
  --

  --*
  -- * \brief       Encode Stream Header
  -- *
  -- * \param       options     Stream Header options to be encoded.
  -- *                          options->backward_size is ignored and doesn't
  -- *                          need to be initialized.
  -- * \param       out         Beginning of the output buffer of
  -- *                          LZMA_STREAM_HEADER_SIZE bytes.
  -- *
  -- * \return      - LZMA_OK: Encoding was successful.
  -- *              - LZMA_OPTIONS_ERROR: options->version is not supported by
  -- *                this liblzma version.
  -- *              - LZMA_PROG_ERROR: Invalid options.
  --

   function lzma_stream_header_encode (options : access constant lzma_stream_flags; c_out : access Ada.Streams.Stream_Element) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/stream_flags.h:122
   pragma Import (C, lzma_stream_header_encode, "lzma_stream_header_encode");

  --*
  -- * \brief       Encode Stream Footer
  -- *
  -- * \param       options     Stream Footer options to be encoded.
  -- * \param       out         Beginning of the output buffer of
  -- *                          LZMA_STREAM_HEADER_SIZE bytes.
  -- *
  -- * \return      - LZMA_OK: Encoding was successful.
  -- *              - LZMA_OPTIONS_ERROR: options->version is not supported by
  -- *                this liblzma version.
  -- *              - LZMA_PROG_ERROR: Invalid options.
  --

   function lzma_stream_footer_encode (options : access constant lzma_stream_flags; c_out : access Ada.Streams.Stream_Element) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/stream_flags.h:139
   pragma Import (C, lzma_stream_footer_encode, "lzma_stream_footer_encode");

  --*
  -- * \brief       Decode Stream Header
  -- *
  -- * \param       options     Target for the decoded Stream Header options.
  -- * \param       in          Beginning of the input buffer of
  -- *                          LZMA_STREAM_HEADER_SIZE bytes.
  -- *
  -- * options->backward_size is always set to LZMA_VLI_UNKNOWN. This is to
  -- * help comparing Stream Flags from Stream Header and Stream Footer with
  -- * lzma_stream_flags_compare().
  -- *
  -- * \return      - LZMA_OK: Decoding was successful.
  -- *              - LZMA_FORMAT_ERROR: Magic bytes don't match, thus the given
  -- *                buffer cannot be Stream Header.
  -- *              - LZMA_DATA_ERROR: CRC32 doesn't match, thus the header
  -- *                is corrupt.
  -- *              - LZMA_OPTIONS_ERROR: Unsupported options are present
  -- *                in the header.
  -- *
  -- * \note        When decoding .xz files that contain multiple Streams, it may
  -- *              make sense to print "file format not recognized" only if
  -- *              decoding of the Stream Header of the _first_ Stream gives
  -- *              LZMA_FORMAT_ERROR. If non-first Stream Header gives
  -- *              LZMA_FORMAT_ERROR, the message used for LZMA_DATA_ERROR is
  -- *              probably more appropriate.
  -- *
  -- *              For example, Stream decoder in liblzma uses LZMA_DATA_ERROR if
  -- *              LZMA_FORMAT_ERROR is returned by lzma_stream_header_decode()
  -- *              when decoding non-first Stream.
  --

   function lzma_stream_header_decode (options : access lzma_stream_flags; c_in : access Ada.Streams.Stream_Element) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/stream_flags.h:174
   pragma Import (C, lzma_stream_header_decode, "lzma_stream_header_decode");

  --*
  -- * \brief       Decode Stream Footer
  -- *
  -- * \param       options     Target for the decoded Stream Header options.
  -- * \param       in          Beginning of the input buffer of
  -- *                          LZMA_STREAM_HEADER_SIZE bytes.
  -- *
  -- * \return      - LZMA_OK: Decoding was successful.
  -- *              - LZMA_FORMAT_ERROR: Magic bytes don't match, thus the given
  -- *                buffer cannot be Stream Footer.
  -- *              - LZMA_DATA_ERROR: CRC32 doesn't match, thus the Stream Footer
  -- *                is corrupt.
  -- *              - LZMA_OPTIONS_ERROR: Unsupported options are present
  -- *                in Stream Footer.
  -- *
  -- * \note        If Stream Header was already decoded successfully, but
  -- *              decoding Stream Footer returns LZMA_FORMAT_ERROR, the
  -- *              application should probably report some other error message
  -- *              than "file format not recognized", since the file more likely
  -- *              is corrupt (possibly truncated). Stream decoder in liblzma
  -- *              uses LZMA_DATA_ERROR in this situation.
  --

   function lzma_stream_footer_decode (options : access lzma_stream_flags; c_in : access Ada.Streams.Stream_Element) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/stream_flags.h:201
   pragma Import (C, lzma_stream_footer_decode, "lzma_stream_footer_decode");

  --*
  -- * \brief       Compare two lzma_stream_flags structures
  -- *
  -- * backward_size values are compared only if both are not
  -- * LZMA_VLI_UNKNOWN.
  -- *
  -- * \return      - LZMA_OK: Both are equal. If either had backward_size set
  -- *                to LZMA_VLI_UNKNOWN, backward_size values were not
  -- *                compared or validated.
  -- *              - LZMA_DATA_ERROR: The structures differ.
  -- *              - LZMA_OPTIONS_ERROR: version in either structure is greater
  -- *                than the maximum supported version (currently zero).
  -- *              - LZMA_PROG_ERROR: Invalid value, e.g. invalid check or
  -- *                backward_size.
  --

   function lzma_stream_flags_compare (a : access constant lzma_stream_flags; b : access constant lzma_stream_flags) return Lzma.Base.lzma_ret;  -- /usr/include/lzma/stream_flags.h:221
   pragma Import (C, lzma_stream_flags_compare, "lzma_stream_flags_compare");

end Lzma.StreamFlags;

pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lzma.Base;
with Ada.Streams;

package Lzma.Check is


   LZMA_CHECK_ID_MAX : constant := 15;  --  /usr/include/lzma/check.h:68

   LZMA_CHECK_SIZE_MAX : constant := 64;  --  /usr/include/lzma/check.h:102

  --*
  -- * \file        lzma/check.h
  -- * \brief       Integrity checks
  --

  -- * Author: Lasse Collin
  -- *
  -- * This file has been put into the public domain.
  -- * You can do whatever you want with this file.
  -- *
  -- * See ../lzma.h for information about liblzma as a whole.
  --

  --*
  -- * \brief       Type of the integrity check (Check ID)
  -- *
  -- * The .xz format supports multiple types of checks that are calculated
  -- * from the uncompressed data. They vary in both speed and ability to
  -- * detect errors.
  --

  --*<
  --   * No Check is calculated.
  --   *
  --   * Size of the Check field: 0 bytes
  --

  --*<
  --   * CRC32 using the polynomial from the IEEE 802.3 standard
  --   *
  --   * Size of the Check field: 4 bytes
  --

  --*<
  --   * CRC64 using the polynomial from the ECMA-182 standard
  --   *
  --   * Size of the Check field: 8 bytes
  --

  --*<
  --   * SHA-256
  --   *
  --   * Size of the Check field: 32 bytes
  --

   subtype lzma_check is unsigned;
   LZMA_CHECK_NONE : constant lzma_check := 0;
   LZMA_CHECK_CRC32 : constant lzma_check := 1;
   LZMA_CHECK_CRC64 : constant lzma_check := 4;
   LZMA_CHECK_SHA256 : constant lzma_check := 10;  -- /usr/include/lzma/check.h:55

  --*
  -- * \brief       Maximum valid Check ID
  -- *
  -- * The .xz file format specification specifies 16 Check IDs (0-15). Some
  -- * of them are only reserved, that is, no actual Check algorithm has been
  -- * assigned. When decoding, liblzma still accepts unknown Check IDs for
  -- * future compatibility. If a valid but unsupported Check ID is detected,
  -- * liblzma can indicate a warning; see the flags LZMA_TELL_NO_CHECK,
  -- * LZMA_TELL_UNSUPPORTED_CHECK, and LZMA_TELL_ANY_CHECK in container.h.
  --

  --*
  -- * \brief       Test if the given Check ID is supported
  -- *
  -- * Return true if the given Check ID is supported by this liblzma build.
  -- * Otherwise false is returned. It is safe to call this with a value that
  -- * is not in the range [0, 15]; in that case the return value is always false.
  -- *
  -- * You can assume that LZMA_CHECK_NONE and LZMA_CHECK_CRC32 are always
  -- * supported (even if liblzma is built with limited features).
  --

   function lzma_check_is_supported (check : lzma_check) return Lzma.Base.lzma_bool;  -- /usr/include/lzma/check.h:81
   pragma Import (C, lzma_check_is_supported, "lzma_check_is_supported");

  --*
  -- * \brief       Get the size of the Check field with the given Check ID
  -- *
  -- * Although not all Check IDs have a check algorithm associated, the size of
  -- * every Check is already frozen. This function returns the size (in bytes) of
  -- * the Check field with the specified Check ID. The values are:
  -- * { 0, 4, 4, 4, 8, 8, 8, 16, 16, 16, 32, 32, 32, 64, 64, 64 }
  -- *
  -- * If the argument is not in the range [0, 15], UINT32_MAX is returned.
  --

   function lzma_check_size (check : lzma_check) return Interfaces.C.unsigned;  -- /usr/include/lzma/check.h:95
   pragma Import (C, lzma_check_size, "lzma_check_size");

  --*
  -- * \brief       Maximum size of a Check field
  --

  --*
  -- * \brief       Calculate CRC32
  -- *
  -- * Calculate CRC32 using the polynomial from the IEEE 802.3 standard.
  -- *
  -- * \param       buf     Pointer to the input buffer
  -- * \param       size    Size of the input buffer
  -- * \param       crc     Previously returned CRC value. This is used to
  -- *                      calculate the CRC of a big buffer in smaller chunks.
  -- *                      Set to zero when starting a new calculation.
  -- *
  -- * \return      Updated CRC value, which can be passed to this function
  -- *              again to continue CRC calculation.
  --

   function lzma_crc32
     (buf : access Ada.Streams.Stream_Element;
      size : Interfaces.C.size_t;
      crc : Interfaces.C.unsigned) return Interfaces.C.unsigned;  -- /usr/include/lzma/check.h:119
   pragma Import (C, lzma_crc32, "lzma_crc32");

  --*
  -- * \brief       Calculate CRC64
  -- *
  -- * Calculate CRC64 using the polynomial from the ECMA-182 standard.
  -- *
  -- * This function is used similarly to lzma_crc32(). See its documentation.
  --

   function lzma_crc64
     (buf : access Ada.Streams.Stream_Element;
      size : Interfaces.C.size_t;
      crc : Long_Long_Integer) return Long_Long_Integer;  -- /usr/include/lzma/check.h:131
   pragma Import (C, lzma_crc64, "lzma_crc64");

  -- * SHA-256 functions are currently not exported to public API.
  -- * Contact Lasse Collin if you think it should be.
  --

  --*
  -- * \brief       Get the type of the integrity check
  -- *
  -- * This function can be called only immediately after lzma_code() has
  -- * returned LZMA_NO_CHECK, LZMA_UNSUPPORTED_CHECK, or LZMA_GET_CHECK.
  -- * Calling this function in any other situation has undefined behavior.
  --

   function lzma_get_check (strm : access constant Lzma.Base.lzma_stream) return lzma_check;  -- /usr/include/lzma/check.h:149
   pragma Import (C, lzma_get_check, "lzma_get_check");

end Lzma.Check;

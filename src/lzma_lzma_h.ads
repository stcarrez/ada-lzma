pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lzma.Base;
with Ada.Streams;
with System;

package lzma_lzma_h is

   --  unsupported macro: LZMA_FILTER_LZMA1 LZMA_VLI_C(0x4000000000000001)
   --  unsupported macro: LZMA_FILTER_LZMA2 LZMA_VLI_C(0x21)
   --  unsupported macro: LZMA_DICT_SIZE_MIN UINT32_C(4096)
   --  unsupported macro: LZMA_DICT_SIZE_DEFAULT (UINT32_C(1) << 23)

   LZMA_LCLP_MIN : constant := 0;  --  /usr/include/lzma/lzma.h:282
   LZMA_LCLP_MAX : constant := 4;  --  /usr/include/lzma/lzma.h:283
   LZMA_LC_DEFAULT : constant := 3;  --  /usr/include/lzma/lzma.h:284

   LZMA_LP_DEFAULT : constant := 0;  --  /usr/include/lzma/lzma.h:294

   LZMA_PB_MIN : constant := 0;  --  /usr/include/lzma/lzma.h:317
   LZMA_PB_MAX : constant := 4;  --  /usr/include/lzma/lzma.h:318
   LZMA_PB_DEFAULT : constant := 2;  --  /usr/include/lzma/lzma.h:319

  --*
  -- * \file        lzma/lzma.h
  -- * \brief       LZMA1 and LZMA2 filters
  --

  -- * Author: Lasse Collin
  -- *
  -- * This file has been put into the public domain.
  -- * You can do whatever you want with this file.
  -- *
  -- * See ../lzma.h for information about liblzma as a whole.
  --

  --*
  -- * \brief       LZMA1 Filter ID
  -- *
  -- * LZMA1 is the very same thing as what was called just LZMA in LZMA Utils,
  -- * 7-Zip, and LZMA SDK. It's called LZMA1 here to prevent developers from
  -- * accidentally using LZMA when they actually want LZMA2.
  -- *
  -- * LZMA1 shouldn't be used for new applications unless you _really_ know
  -- * what you are doing. LZMA2 is almost always a better choice.
  --

  --*
  -- * \brief       LZMA2 Filter ID
  -- *
  -- * Usually you want this instead of LZMA1. Compared to LZMA1, LZMA2 adds
  -- * support for LZMA_SYNC_FLUSH, uncompressed chunks (smaller expansion
  -- * when trying to compress uncompressible data), possibility to change
  -- * lc/lp/pb in the middle of encoding, and some other internal improvements.
  --

  --*
  -- * \brief       Match finders
  -- *
  -- * Match finder has major effect on both speed and compression ratio.
  -- * Usually hash chains are faster than binary trees.
  -- *
  -- * If you will use LZMA_SYNC_FLUSH often, the hash chains may be a better
  -- * choice, because binary trees get much higher compression ratio penalty
  -- * with LZMA_SYNC_FLUSH.
  -- *
  -- * The memory usage formulas are only rough estimates, which are closest to
  -- * reality when dict_size is a power of two. The formulas are  more complex
  -- * in reality, and can also change a little between liblzma versions. Use
  -- * lzma_raw_encoder_memusage() to get more accurate estimate of memory usage.
  --

  --*<
  --   * \brief       Hash Chain with 2- and 3-byte hashing
  --   *
  --   * Minimum nice_len: 3
  --   *
  --   * Memory usage:
  --   *  - dict_size <= 16 MiB: dict_size * 7.5
  --   *  - dict_size > 16 MiB: dict_size * 5.5 + 64 MiB
  --

  --*<
  --   * \brief       Hash Chain with 2-, 3-, and 4-byte hashing
  --   *
  --   * Minimum nice_len: 4
  --   *
  --   * Memory usage:
  --   *  - dict_size <= 32 MiB: dict_size * 7.5
  --   *  - dict_size > 32 MiB: dict_size * 6.5
  --

  --*<
  --   * \brief       Binary Tree with 2-byte hashing
  --   *
  --   * Minimum nice_len: 2
  --   *
  --   * Memory usage: dict_size * 9.5
  --

  --*<
  --   * \brief       Binary Tree with 2- and 3-byte hashing
  --   *
  --   * Minimum nice_len: 3
  --   *
  --   * Memory usage:
  --   *  - dict_size <= 16 MiB: dict_size * 11.5
  --   *  - dict_size > 16 MiB: dict_size * 9.5 + 64 MiB
  --

  --*<
  --   * \brief       Binary Tree with 2-, 3-, and 4-byte hashing
  --   *
  --   * Minimum nice_len: 4
  --   *
  --   * Memory usage:
  --   *  - dict_size <= 32 MiB: dict_size * 11.5
  --   *  - dict_size > 32 MiB: dict_size * 10.5
  --

   subtype lzma_match_finder is unsigned;
   LZMA_MF_HC3 : constant lzma_match_finder := 3;
   LZMA_MF_HC4 : constant lzma_match_finder := 4;
   LZMA_MF_BT2 : constant lzma_match_finder := 18;
   LZMA_MF_BT3 : constant lzma_match_finder := 19;
   LZMA_MF_BT4 : constant lzma_match_finder := 20;  -- /usr/include/lzma/lzma.h:111

  --*
  -- * \brief       Test if given match finder is supported
  -- *
  -- * Return true if the given match finder is supported by this liblzma build.
  -- * Otherwise false is returned. It is safe to call this with a value that
  -- * isn't listed in lzma_match_finder enumeration; the return value will be
  -- * false.
  -- *
  -- * There is no way to list which match finders are available in this
  -- * particular liblzma version and build. It would be useless, because
  -- * a new match finder, which the application developer wasn't aware,
  -- * could require giving additional options to the encoder that the older
  -- * match finders don't need.
  --

   function lzma_mf_is_supported (match_finder : lzma_match_finder) return Lzma.Base.lzma_bool;  -- /usr/include/lzma/lzma.h:128
   pragma Import (C, lzma_mf_is_supported, "lzma_mf_is_supported");

  --*
  -- * \brief       Compression modes
  -- *
  -- * This selects the function used to analyze the data produced by the match
  -- * finder.
  --

  --*<
  --   * \brief       Fast compression
  --   *
  --   * Fast mode is usually at its best when combined with
  --   * a hash chain match finder.
  --

  --*<
  --   * \brief       Normal compression
  --   *
  --   * This is usually notably slower than fast mode. Use this
  --   * together with binary tree match finders to expose the
  --   * full potential of the LZMA1 or LZMA2 encoder.
  --

   subtype lzma_mode is unsigned;
   LZMA_MODE_FAST : constant lzma_mode := 1;
   LZMA_MODE_NORMAL : constant lzma_mode := 2;  -- /usr/include/lzma/lzma.h:155

  --*
  -- * \brief       Test if given compression mode is supported
  -- *
  -- * Return true if the given compression mode is supported by this liblzma
  -- * build. Otherwise false is returned. It is safe to call this with a value
  -- * that isn't listed in lzma_mode enumeration; the return value will be false.
  -- *
  -- * There is no way to list which modes are available in this particular
  -- * liblzma version and build. It would be useless, because a new compression
  -- * mode, which the application developer wasn't aware, could require giving
  -- * additional options to the encoder that the older modes don't need.
  --

   function lzma_mode_is_supported (mode : lzma_mode) return Lzma.Base.lzma_bool;  -- /usr/include/lzma/lzma.h:170
   pragma Import (C, lzma_mode_is_supported, "lzma_mode_is_supported");

  --*
  -- * \brief       Options specific to the LZMA1 and LZMA2 filters
  -- *
  -- * Since LZMA1 and LZMA2 share most of the code, it's simplest to share
  -- * the options structure too. For encoding, all but the reserved variables
  -- * need to be initialized unless specifically mentioned otherwise.
  -- * lzma_lzma_preset() can be used to get a good starting point.
  -- *
  -- * For raw decoding, both LZMA1 and LZMA2 need dict_size, preset_dict, and
  -- * preset_dict_size (if preset_dict != NULL). LZMA1 needs also lc, lp, and pb.
  --

  --*
  --  * \brief       Dictionary size in bytes
  --  *
  --  * Dictionary size indicates how many bytes of the recently processed
  --  * uncompressed data is kept in memory. One method to reduce size of
  --  * the uncompressed data is to store distance-length pairs, which
  --  * indicate what data to repeat from the dictionary buffer. Thus,
  --  * the bigger the dictionary, the better the compression ratio
  --  * usually is.
  --  *
  --  * Maximum size of the dictionary depends on multiple things:
  --  *  - Memory usage limit
  --  *  - Available address space (not a problem on 64-bit systems)
  --  *  - Selected match finder (encoder only)
  --  *
  --  * Currently the maximum dictionary size for encoding is 1.5 GiB
  --  * (i.e. (UINT32_C(1) << 30) + (UINT32_C(1) << 29)) even on 64-bit
  --  * systems for certain match finder implementation reasons. In the
  --  * future, there may be match finders that support bigger
  --  * dictionaries.
  --  *
  --  * Decoder already supports dictionaries up to 4 GiB - 1 B (i.e.
  --  * UINT32_MAX), so increasing the maximum dictionary size of the
  --  * encoder won't cause problems for old decoders.
  --  *
  --  * Because extremely small dictionaries sizes would have unneeded
  --  * overhead in the decoder, the minimum dictionary size is 4096 bytes.
  --  *
  --  * \note        When decoding, too big dictionary does no other harm
  --  *              than wasting memory.
  --

   type lzma_options_lzma is record
      dict_size : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:217
      preset_dict : access Ada.Streams.Stream_Element;  -- /usr/include/lzma/lzma.h:240
      preset_dict_size : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:254
      lc : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:281
      lp : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:293
      pb : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:316
      mode : aliased lzma_mode;  -- /usr/include/lzma/lzma.h:322
      nice_len : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:342
      mf : aliased lzma_match_finder;  -- /usr/include/lzma/lzma.h:345
      depth : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:375
      reserved_int1 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:384
      reserved_int2 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:385
      reserved_int3 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:386
      reserved_int4 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:387
      reserved_int5 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:388
      reserved_int6 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:389
      reserved_int7 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:390
      reserved_int8 : aliased Interfaces.C.unsigned;  -- /usr/include/lzma/lzma.h:391
      reserved_enum1 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/lzma.h:392
      reserved_enum2 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/lzma.h:393
      reserved_enum3 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/lzma.h:394
      reserved_enum4 : aliased Lzma.Base.lzma_reserved_enum_type;  -- /usr/include/lzma/lzma.h:395
      reserved_ptr1 : System.Address;  -- /usr/include/lzma/lzma.h:396
      reserved_ptr2 : System.Address;  -- /usr/include/lzma/lzma.h:397
   end record;
   pragma Convention (C_Pass_By_Copy, lzma_options_lzma);  -- /usr/include/lzma/lzma.h:399

   --  skipped anonymous struct anon_13

  --*
  --  * \brief       Pointer to an initial dictionary
  --  *
  --  * It is possible to initialize the LZ77 history window using
  --  * a preset dictionary. It is useful when compressing many
  --  * similar, relatively small chunks of data independently from
  --  * each other. The preset dictionary should contain typical
  --  * strings that occur in the files being compressed. The most
  --  * probable strings should be near the end of the preset dictionary.
  --  *
  --  * This feature should be used only in special situations. For
  --  * now, it works correctly only with raw encoding and decoding.
  --  * Currently none of the container formats supported by
  --  * liblzma allow preset dictionary when decoding, thus if
  --  * you create a .xz or .lzma file with preset dictionary, it
  --  * cannot be decoded with the regular decoder functions. In the
  --  * future, the .xz format will likely get support for preset
  --  * dictionary though.
  --

  --*
  --  * \brief       Size of the preset dictionary
  --  *
  --  * Specifies the size of the preset dictionary. If the size is
  --  * bigger than dict_size, only the last dict_size bytes are
  --  * processed.
  --  *
  --  * This variable is read only when preset_dict is not NULL.
  --  * If preset_dict is not NULL but preset_dict_size is zero,
  --  * no preset dictionary is used (identical to only setting
  --  * preset_dict to NULL).
  --

  --*
  --  * \brief       Number of literal context bits
  --  *
  --  * How many of the highest bits of the previous uncompressed
  --  * eight-bit byte (also known as `literal') are taken into
  --  * account when predicting the bits of the next literal.
  --  *
  --  * E.g. in typical English text, an upper-case letter is
  --  * often followed by a lower-case letter, and a lower-case
  --  * letter is usually followed by another lower-case letter.
  --  * In the US-ASCII character set, the highest three bits are 010
  --  * for upper-case letters and 011 for lower-case letters.
  --  * When lc is at least 3, the literal coding can take advantage of
  --  * this property in the uncompressed data.
  --  *
  --  * There is a limit that applies to literal context bits and literal
  --  * position bits together: lc + lp <= 4. Without this limit the
  --  * decoding could become very slow, which could have security related
  --  * results in some cases like email servers doing virus scanning.
  --  * This limit also simplifies the internal implementation in liblzma.
  --  *
  --  * There may be LZMA1 streams that have lc + lp > 4 (maximum possible
  --  * lc would be 8). It is not possible to decode such streams with
  --  * liblzma.
  --

  --*
  --  * \brief       Number of literal position bits
  --  *
  --  * lp affects what kind of alignment in the uncompressed data is
  --  * assumed when encoding literals. A literal is a single 8-bit byte.
  --  * See pb below for more information about alignment.
  --

  --*
  --  * \brief       Number of position bits
  --  *
  --  * pb affects what kind of alignment in the uncompressed data is
  --  * assumed in general. The default means four-byte alignment
  --  * (2^ pb =2^2=4), which is often a good choice when there's
  --  * no better guess.
  --  *
  --  * When the aligment is known, setting pb accordingly may reduce
  --  * the file size a little. E.g. with text files having one-byte
  --  * alignment (US-ASCII, ISO-8859-*, UTF-8), setting pb=0 can
  --  * improve compression slightly. For UTF-16 text, pb=1 is a good
  --  * choice. If the alignment is an odd number like 3 bytes, pb=0
  --  * might be the best choice.
  --  *
  --  * Even though the assumed alignment can be adjusted with pb and
  --  * lp, LZMA1 and LZMA2 still slightly favor 16-byte alignment.
  --  * It might be worth taking into account when designing file formats
  --  * that are likely to be often compressed with LZMA1 or LZMA2.
  --

  --* Compression mode
  --*
  --  * \brief       Nice length of a match
  --  *
  --  * This determines how many bytes the encoder compares from the match
  --  * candidates when looking for the best match. Once a match of at
  --  * least nice_len bytes long is found, the encoder stops looking for
  --  * better candidates and encodes the match. (Naturally, if the found
  --  * match is actually longer than nice_len, the actual length is
  --  * encoded; it's not truncated to nice_len.)
  --  *
  --  * Bigger values usually increase the compression ratio and
  --  * compression time. For most files, 32 to 128 is a good value,
  --  * which gives very good compression ratio at good speed.
  --  *
  --  * The exact minimum value depends on the match finder. The maximum
  --  * is 273, which is the maximum length of a match that LZMA1 and
  --  * LZMA2 can encode.
  --

  --* Match finder ID
  --*
  --  * \brief       Maximum search depth in the match finder
  --  *
  --  * For every input byte, match finder searches through the hash chain
  --  * or binary tree in a loop, each iteration going one step deeper in
  --  * the chain or tree. The searching stops if
  --  *  - a match of at least nice_len bytes long is found;
  --  *  - all match candidates from the hash chain or binary tree have
  --  *    been checked; or
  --  *  - maximum search depth is reached.
  --  *
  --  * Maximum search depth is needed to prevent the match finder from
  --  * wasting too much time in case there are lots of short match
  --  * candidates. On the other hand, stopping the search before all
  --  * candidates have been checked can reduce compression ratio.
  --  *
  --  * Setting depth to zero tells liblzma to use an automatic default
  --  * value, that depends on the selected match finder and nice_len.
  --  * The default is in the range [4, 200] or so (it may vary between
  --  * liblzma versions).
  --  *
  --  * Using a bigger depth value than the default can increase
  --  * compression ratio in some cases. There is no strict maximum value,
  --  * but high values (thousands or millions) should be used with care:
  --  * the encoder could remain fast enough with typical input, but
  --  * malicious input could cause the match finder to slow down
  --  * dramatically, possibly creating a denial of service attack.
  --

  --  * Reserved space to allow possible future extensions without
  --  * breaking the ABI. You should not touch these, because the names
  --  * of these variables may change. These are and will never be used
  --  * with the currently supported options, so it is safe to leave these
  --  * uninitialized.
  --

  --*
  -- * \brief       Set a compression preset to lzma_options_lzma structure
  -- *
  -- * 0 is the fastest and 9 is the slowest. These match the switches -0 .. -9
  -- * of the xz command line tool. In addition, it is possible to bitwise-or
  -- * flags to the preset. Currently only LZMA_PRESET_EXTREME is supported.
  -- * The flags are defined in container.h, because the flags are used also
  -- * with lzma_easy_encoder().
  -- *
  -- * The preset values are subject to changes between liblzma versions.
  -- *
  -- * This function is available only if LZMA1 or LZMA2 encoder has been enabled
  -- * when building liblzma.
  -- *
  -- * \return      On success, false is returned. If the preset is not
  -- *              supported, true is returned.
  --

   function lzma_lzma_preset (options : access lzma_options_lzma; preset : Interfaces.C.unsigned) return Lzma.Base.lzma_bool;  -- /usr/include/lzma/lzma.h:419
   pragma Import (C, lzma_lzma_preset, "lzma_lzma_preset");

end lzma_lzma_h;

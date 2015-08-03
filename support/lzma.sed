/with stddef_h;/d
s/with stdint_h;/with Ada.Streams;/
s/stddef_h.size_t/Interfaces.C.size_t/g
s/aliased stdint_h.uint8_t/aliased Ada.Streams.Stream_Element/g
s/stdint_h.uint8_t/Ada.Streams.Stream_Element/g
s/stdint_h.uint32_t/Interfaces.C.unsigned/g
s/stdint_h.uint64_t/Long_Long_Integer/g
s/lzma_base_h/Lzma.Base/g
s/lzma_check_h/Lzma.Check/g
s/lzma_filter_h/Lzma.Filter/g
s/lzma_vli_h/Lzma.Vli/g
s/lzma_container_h/Lzma.Container/g
s/lzma_stream_flags_h/Lzma.StreamFlags/g
s/lzma_reserved_enum/lzma_reserved_enum_type/g
s/\t/ /g
s/ *$//g
/lzma_block_header/ b skip
s/lzma_block_h/Lzma.Block/g
: skip
s/pragma Convention (C_Pass_By_Copy, lzma_stream);/pragma Convention (C_Pass_By_Copy, lzma_stream);\
\
   LZMA_STREAM_INIT : constant lzma_stream :=\
     (next_in => null, avail_in => 0, total_in => 0,\
      next_out => null, avail_out => 0, total_out => 0, allocator => null,\
      internal => System.Null_Address, reserved_ptr1 => System.Null_Address,\
      reserved_ptr2 => System.Null_Address, reserved_ptr3 => System.Null_Address,\
      reserved_ptr4 => System.Null_Address, reserved_enum1 => LZMA_RESERVED_ENUM,\
      reserved_enum2 => LZMA_RESERVED_ENUM, reserved_int1 => 0,\
      reserved_int2 => 0, others => 0);/




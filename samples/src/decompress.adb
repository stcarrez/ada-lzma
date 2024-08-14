-----------------------------------------------------------------------
--  decompress - Decompress example based on the 02_decompress.c example
--  The MIT License (MIT)
--
--  Copyright (c) 2015 Stephane Carrez
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;

with Interfaces.C;

with Lzma;
with Lzma.Base;
with Lzma.Container;
procedure Decompress is

   use type Interfaces.C.size_t;
   use type Lzma.Base.lzma_ret;

   procedure Init_Decoder;
   procedure Decompress (Source : in String;
                         Dest   : in String);

   BUFSIZE : constant Ada.Streams.Stream_Element_Offset := 4096;

   Stream  : aliased Lzma.Base.lzma_stream := Lzma.Base.LZMA_STREAM_INIT;

   procedure Init_Decoder is
      Result : Lzma.Base.lzma_ret;
   begin
      Result := Lzma.Container.lzma_stream_decoder (Stream'Unchecked_Access,
                                                    Long_Long_Integer'Last,
                                                    Lzma.Container.LZMA_CONCATENATED);
      if Result /= Lzma.Base.LZMA_OK then
         Ada.Text_IO.Put_Line ("Error initializing the decoder: "
                                 & Lzma.Base.lzma_ret'Image (Result));
      end if;
   end Init_Decoder;

   --  ------------------------------
   --  Open the source file for reading, decompress that file and write the decompressed
   --  output in the destination file.
   --  ------------------------------
   procedure Decompress (Source : in String;
                         Dest   : in String) is

      Infile     : Ada.Streams.Stream_IO.File_Type;
      Outfile    : Ada.Streams.Stream_IO.File_Type;
      Sbuf       : aliased Ada.Streams.Stream_Element_Array (1 .. BUFSIZE);
      Dbuf       : aliased Ada.Streams.Stream_Element_Array (1 .. BUFSIZE);
      Action     : Lzma.Base.lzma_action := Lzma.Base.LZMA_RUN;
      Last       : Ada.Streams.Stream_Element_Offset;
      Result     : Lzma.Base.lzma_ret;
   begin
      Ada.Streams.Stream_IO.Open (Infile, Ada.Streams.Stream_IO.In_File, Source);
      Ada.Streams.Stream_IO.Create (Outfile, Ada.Streams.Stream_IO.Out_File, Dest);

      Stream.next_out  := Dbuf (Dbuf'First)'Unchecked_Access;
      Stream.avail_out := Dbuf'Length;
      loop
         --  Read a block of data from the source file.
         if Stream.avail_in = 0 and not Ada.Streams.Stream_IO.End_Of_File (Infile) then
            Stream.next_in := Sbuf (Sbuf'First)'Unchecked_Access;
            Ada.Streams.Stream_IO.Read (Infile, Sbuf, Last);
            Stream.avail_in := Interfaces.C.size_t (Last);
            if Ada.Streams.Stream_IO.End_Of_File (Infile) then
               Action := Lzma.Base.LZMA_FINISH;
            end if;
         end if;
         Result := Lzma.Base.lzma_code (Stream'Unchecked_Access, Action);

         --  Write the output data when the buffer is full or we reached the end of stream.
         if Stream.avail_out = 0 or Result = Lzma.Base.LZMA_STREAM_END then
            Last := Ada.Streams.Stream_Element_Offset (Dbuf'Length - Stream.avail_out);
            Ada.Streams.Stream_IO.Write (Outfile, Item => Dbuf (Dbuf'First .. Last));
            Stream.next_out := Dbuf (Dbuf'First)'Unchecked_Access;
            Stream.avail_out := Dbuf'Length;
         end if;
         exit when Result /= Lzma.Base.LZMA_OK;
      end loop;
      Ada.Streams.Stream_IO.Close (Infile);
      Ada.Streams.Stream_IO.Close (Outfile);
      if Result /= Lzma.Base.LZMA_STREAM_END then
         Ada.Text_IO.Put_Line ("Error while decompressing the input stream: "
                              & Lzma.Base.lzma_ret'Image (Result));
      end if;
   end Decompress;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: decompress input output.xz");
      return;
   end if;
   Init_Decoder;
   Decompress (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2));
   Lzma.Base.lzma_end (Stream'Unchecked_Access);
end Decompress;

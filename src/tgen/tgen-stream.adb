------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body TGen.Stream is

   procedure Free_Buffer is
     new Ada.Unchecked_Deallocation (Object => Stream_Element_Array,
                                     Name   => Buffer_Access);

   procedure Read
     (Stream : in out Buffer_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Chunk_Size : constant Stream_Element_Offset := Item'Length;
   begin
      if Stream.Read_Pos + Chunk_Size > Stream.Buffer'Last then
         raise Program_Error with "buffer overflow";
      end if;
      Item (Item'First .. Item'Last) :=
        Stream.Buffer (Stream.Read_Pos .. Stream.Read_Pos + Chunk_Size - 1);
      Stream.Read_Pos := Stream.Read_Pos + Chunk_Size;
      Last := Stream.Read_Pos;
   end Read;

   procedure Flush (Stream : in out Buffer_Stream) is
   begin
      Stream.Write_Pos := 1;
      Stream.Read_Pos := 1;
      Stream.Written_Chunk_Size := 0;
   end Flush;

   procedure Initialize (Stream : out Buffer_Stream) is
   begin
      if Stream.Buffer /= null then
         Free_Buffer (Stream.Buffer);
      end if;

      --  initial size : 1024. TODO: reallocate mem if the content to store
      --  does not fit.

      Stream.Buffer := new Stream_Element_Array (1 .. 1024);
      Stream.Write_Pos := 1;
      Stream.Read_Pos := 1;
   end Initialize;

   --  ------------------------------
   --  Flush the stream and release the buffer.
   --  ------------------------------
   procedure Finalize (Object : in out Buffer_Stream) is
   begin
      if Object.Buffer /= null then
         Free_Buffer (Object.Buffer);
      end if;
   end Finalize;

   procedure Write
     (Stream : in out Buffer_Stream;
      Item : Ada.Streams.Stream_Element_Array)
   is
      Chunk_Size : constant Stream_Element_Offset := Item'Length;
   begin
      if Stream.Write_Pos + Chunk_Size > Stream.Buffer'Last then
         raise Program_Error with "buffer overflow";
      end if;
      Stream.Buffer (Stream.Write_Pos .. Stream.Write_Pos + Chunk_Size - 1) :=
        Item;
      Stream.Write_Pos := Stream.Write_Pos + Chunk_Size;
      Stream.Written_Chunk_Size := Chunk_Size;
   end Write;

end TGen.Stream;

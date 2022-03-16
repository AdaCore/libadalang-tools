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
--
--  Stream utilities to communicate between the value generation units and the
--  test execution engine.

with Ada.Streams; use Ada.Streams;

package TGen.Stream is

   type Buffer_Access is access Ada.Streams.Stream_Element_Array;

   type Flushable_Stream is abstract limited new Root_Stream_Type
   with null record;

   procedure Flush (Stream : in out Flushable_Stream) is abstract;

   type Buffer_Stream is new Flushable_Stream with private;

   procedure Initialize (Stream : out Buffer_Stream);
   --  Allocate a buffer for the stream

   procedure Finalize (Object : in out Buffer_Stream);
   --  Release the buffer.

   procedure Read
     (Stream : in out Buffer_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  Read the buffer array from the stream.

   procedure Write (Stream : in out Buffer_Stream;
                    Item : Ada.Streams.Stream_Element_Array);
   --  Write the buffer array to the output stream.

   overriding procedure Flush (Stream : in out Buffer_Stream);
   --  Flush the stream

private

   type Buffer_Stream is new Flushable_Stream with record
      --  The buffer where the data is written before being flushed.
      Buffer      : Buffer_Access := null;

      --  Position of last-written element.
      Write_Pos : Stream_Element_Offset := 1;

      Read_Pos : Stream_Element_Offset := 1;

      Written_Chunk_Size : Stream_Element_Offset := 0;
   end record with Type_Invariant => Read_Pos <= Write_Pos;

end TGen.Stream;

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

with Ada.Streams;
with Interfaces; use Interfaces;

package TAGAda_Marshalling_Lib is

   Invalid_Value : exception;

   type Offset_Type is mod 8;

   procedure Write_Padding
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Size   : Natural);

   procedure Read_Padding
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Size   : Natural);

   generic
      type T is (<>);
   package Read_Write_Discrete is

      function Size
        (First : T := T'First;
         Last  : T := T'Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T'First;
         Last   : T := T'Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T'First;
         Last   : T := T'Last);

   end Read_Write_Discrete;

   generic
      type T is delta <> digits <>;
   package Read_Write_Decimal_Fixed is

      function Size
        (First : T := T'First;
         Last  : T := T'Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T'First;
         Last   : T := T'Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T'First;
         Last   : T := T'Last);

   end Read_Write_Decimal_Fixed;

   generic
      type T is delta <>;
   package Read_Write_Ordinary_Fixed is

      function Size
        (First : T := T'First;
         Last  : T := T'Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T'First;
         Last   : T := T'Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T'First;
         Last   : T := T'Last);

   end Read_Write_Ordinary_Fixed;

   generic
      type T is digits <>;
   package Read_Write_Float is

      pragma Compile_Time_Error
        ((case T'Machine_Mantissa is
            when 24     => Float'Machine_Mantissa /= 24,
            when 53     => Long_Float'Machine_Mantissa /= 53,
            when 64     => Long_Long_Float'Machine_Mantissa /= 64,
            when others => True),
         "Unsupported floating-point type");
      --  We only support standard floating point formats in targets that
      --  support them.

      function Size
        (First : T := T'First;
         Last  : T := T'Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T'First;
         Last   : T := T'Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T'First;
         Last   : T := T'Last);

   end Read_Write_Float;

   generic
      type T is private;

      with procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T);

      with procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T);

   package In_Out is

      function Input
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return T;

      procedure Output
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         V      : T);

   end In_Out;

   generic
      type T (<>) is private;
      type Header is private;

      with function Init (H : Header) return T;

      with procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T);

      with procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T);

   package In_Out_Unconstrained is

      function Input
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         H      : Header)
        return T;

      procedure Output
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         V      : T);

   end In_Out_Unconstrained;

end TAGAda_Marshalling_Lib;

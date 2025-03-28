------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2022-2023, AdaCore                      --
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
with System;

package TGen.Marshalling_Lib is

   Invalid_Value : exception;
   --  Exception raised by the marshallers when reading an invalid value from
   --  a stream.

   type Offset_Type is mod 8;

   type Biggest_Int is mod System.Max_Binary_Modulus;

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
      T_First : T;
      T_Last : T;
   package Read_Write_Enum is

      function Size (First : T := T_First; Last : T := T_Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T_First;
         Last   : T := T_Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T_First;
         Last   : T := T_Last);

   end Read_Write_Enum;

   generic
      type T is range <>;
      T_First : T;
      T_Last : T;
   package Read_Write_Signed is

      function Size (First : T := T_First; Last : T := T_Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T_First;
         Last   : T := T_Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T_First;
         Last   : T := T_Last);

   end Read_Write_Signed;

   generic
      type T is mod <>;
      T_First : T;
      T_Last : T;
   package Read_Write_Unsigned is

      function Size (First : T := T_First; Last : T := T_Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T_First;
         Last   : T := T_Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T_First;
         Last   : T := T_Last);

   end Read_Write_Unsigned;

   generic
      type T is delta <> digits <>;
      T_First : T;
      T_Last : T;
   package Read_Write_Decimal_Fixed is

      function Size (First : T := T_First; Last : T := T_Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T_First;
         Last   : T := T_Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T_First;
         Last   : T := T_Last);

   end Read_Write_Decimal_Fixed;

   generic
      type T is delta <>;
      T_First : T;
      T_Last : T;
   package Read_Write_Ordinary_Fixed is
      --!format on

      function Size (First : T := T_First; Last : T := T_Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T_First;
         Last   : T := T_Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T_First;
         Last   : T := T_Last);

   end Read_Write_Ordinary_Fixed;

   --!format off
   generic
      type T is digits <>;
      T_First : T;
      T_Last : T;
   package Read_Write_Float is

      pragma
        Compile_Time_Error
          ((case T'Machine_Mantissa is
              when 24 => Float'Machine_Mantissa /= 24,
              when 53 => Long_Float'Machine_Mantissa /= 53,
              when 64 => Long_Long_Float'Machine_Mantissa /= 64,
              when others => True),
           "Unsupported floating-point type");
      --  We only support standard floating point formats in targets that
      --  support them.

      function Size (First : T := T_First; Last : T := T_Last) return Natural;
      --  Return the number of bits used for values of the type

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T;
         First  : T := T_First;
         Last   : T := T_Last);

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T;
         First  : T := T_First;
         Last   : T := T_Last);

   end Read_Write_Float;

   generic
      type T is private;

      with
        procedure Write
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           Buffer : in out Unsigned_8;
           Offset : in out Offset_Type;
           V      : T);

      with
        procedure Read
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           Buffer : in out Unsigned_8;
           Offset : in out Offset_Type;
           V      : out T);

   package In_Out
   is

      function Input
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T;

      procedure Output
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class; V : T);

      function Input
        (Header : not null access Ada.Streams.Root_Stream_Type'Class;
         Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T;

      procedure Output
        (Header : not null access Ada.Streams.Root_Stream_Type'Class;
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         V      : T);

   end In_Out;

   generic
      type T (<>) is private;
      type Header_Type is private;

      with function Init (H : Header_Type) return T;

      with
        function Input_Header
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
           return Header_Type;

      with
        procedure Output_Header
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class; V : T);

      with
        procedure Write
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           Buffer : in out Unsigned_8;
           Offset : in out Offset_Type;
           V      : T);

      with
        procedure Read
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           Buffer : in out Unsigned_8;
           Offset : in out Offset_Type;
           V      : out T);

   package In_Out_Unconstrained
   is

      function Input
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T;

      procedure Output
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class; V : T);

      function Input
        (Header : not null access Ada.Streams.Root_Stream_Type'Class;
         Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T;

      procedure Output
        (Header : not null access Ada.Streams.Root_Stream_Type'Class;
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         V      : T);

   end In_Out_Unconstrained;

end TGen.Marshalling_Lib;

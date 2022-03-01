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

with Ada.Unchecked_Conversion;
with System.Unsigned_Types; use System.Unsigned_Types;

package body TAGAda_Marshalling_Lib is

   function Size (V : Unsigned_8) return Offset_Type is
     (case V is
         when 0           => 0,
         when 1           => 1,
         when 2 .. 3      => 2,
         when 4 .. 7      => 3,
         when 8 .. 15     => 4,
         when 16 .. 31    => 5,
         when 32 .. 63    => 6,
         when 64 .. 127   => 7,
         when 128 .. 255  => 0);

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  To marshall a scalar values/components, it is transformed into a value
   --  of the biggest unsigned number in the architecture
   --  (Long_Long_Long_Unsigned). Only the relevant bits of the values are
   --  marshalled.

   procedure Write_Remainder
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Num    : Offset_Type;
      V      : Unsigned_8);

   procedure Read_Remainder
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Num    : Offset_Type;
      V      : out Unsigned_8);

   procedure Write
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer   : in out Unsigned_8;
      Offset   : in out Offset_Type;
      Max, Val : Long_Long_Long_Unsigned);

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Max    : Long_Long_Long_Unsigned;
      Val    : out Long_Long_Long_Unsigned);

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Max    : Long_Long_Long_Unsigned;
      Val    : out Long_Long_Long_Unsigned)
   is
      M    : Long_Long_Long_Unsigned := Max;
      Base : Long_Long_Long_Unsigned := 1;

   begin
      Val := 0;

      --  Try to read from the buffer if it is not empty
      if Offset > 0 then
         declare
            Byte : Unsigned_8;
            Num  : constant Offset_Type :=
              (if M < 128 then Offset_Type'Min (-Offset, Size (Unsigned_8 (M)))
               else -Offset);
         begin
            Read_Remainder (Stream, Buffer, Offset, Num, Byte);
            Val := Long_Long_Long_Unsigned (Byte);
            M := Shift_Right (M, Natural (Num));
            Base := 2 ** Natural (Num);
         end;
      end if;

      --  Read complete bytes from the stream
      while M > 127 loop
         declare
            Byte : Unsigned_8;
         begin
            Unsigned_8'Read (Stream, Byte);
            Val := Val + Base * Long_Long_Long_Unsigned (Byte);
            Base := Shift_Left (Base, 8);
            M := Shift_Right (M, 8);
         end;
      end loop;

      --  Read the remaining bits from the buffer
      if M > 0 then
         declare
            Byte  : Unsigned_8;
         begin
            Read_Remainder (Stream, Buffer, Offset, Size (Unsigned_8 (M)), Byte);
            Val := Val + Base * Long_Long_Long_Unsigned (Byte);
         end;
      end if;

      --  Saturate if necessary
      Val := Long_Long_Long_Unsigned'Min (Val, Max);
   end Read;

   --------------------
   -- Read_Remainder --
   --------------------

   procedure Read_Remainder
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Num    : Offset_Type;
      V      : out Unsigned_8)
   is
   begin
      --  If the buffer is empty, read from the stream
      if Offset = 0 then
         Unsigned_8'Read (Stream, Buffer);
      end if;

      --  Put in V the part of Buffer after Offset
      V := Shift_Right (Buffer, Natural (Offset));

      --  If we have more bits to read after the end of Buffer, read a new
      --  value from the stream and add it at the end of V.
      if -Num < Offset then
         Unsigned_8'Read (Stream, Buffer);
         V := V or Shift_Left (Buffer, Natural (- Offset));
      end if;

      --  Truncate V to its expected length
      V := V and (2 ** Natural (Num) - 1);
      Offset := Offset + Num;
   end Read_Remainder;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer   : in out Unsigned_8;
      Offset   : in out Offset_Type;
      Max, Val : Long_Long_Long_Unsigned)
   is
      M : Long_Long_Long_Unsigned := Max;
      V : Long_Long_Long_Unsigned := Val;
   begin
      --  If there are some bits in the buffer, first try to complete it
      if Offset /= 0 then
         declare
            Num  : constant Offset_Type :=
              (if M < 128 then Offset_Type'Min (-Offset, Size (Unsigned_8 (M)))
               else -Offset);
            Mask : constant Long_Long_Long_Unsigned := 2 ** Natural (Num) - 1;
         begin
            Write_Remainder
              (Stream, Buffer, Offset, Num, Unsigned_8 (V and Mask));
            V := Shift_Right (V, Natural (Num));
            M := Shift_Right (M, Natural (Num));
         end;
      end if;

      --  Write complete bytes to stream
      while M >= 128 loop
         Unsigned_8'Write (Stream, Unsigned_8 (V and 255));
         V := Shift_Right (V, 8);
         M := Shift_Right (M, 8);
      end loop;

      --  Add the remaining bits to Buffer
      if M /= 0 then
         Write_Remainder
           (Stream, Buffer, Offset, Size (Unsigned_8 (M)), Unsigned_8 (V));
      end if;
   end Write;

   ---------------------
   -- Write_Remainder --
   ---------------------

   procedure Write_Remainder
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Unsigned_8;
      Offset : in out Offset_Type;
      Num    : Offset_Type;
      V      : Unsigned_8)
   is
   begin
      --  Write V in Buffer after Offset
      Buffer := Buffer or Shift_Left (V, Natural (Offset));

      --  If we have reached the end of Buffer, flush it to Stream and start
      --  a new buffer with the remaining of V.
      if Num > 7 - Offset then
         Unsigned_8'Write (Stream, Buffer);
         Buffer := Shift_Right (V, Natural (-Offset));
      end if;

      Offset := Offset + Num;
   end Write_Remainder;

   -------------------------
   -- Read_Write_Discrete --
   -------------------------

   package body Read_Write_Discrete is

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Norm (V : T) return Long_Long_Long_Unsigned is
        (Long_Long_Long_Unsigned'Val (T'Pos (V) - T'Pos (T'First)));

      function Denorm (V : Long_Long_Long_Unsigned) return T'Base is
        (T'Val (T'Pos (T'First) + Long_Long_Long_Unsigned'Pos (V)));

      -----------
      -- Size --
      -----------

      function Size return Natural is
         Max : Long_Long_Long_Unsigned := Norm (T'Last);
      begin
         for I in 0 .. 128 loop
            if Max = 0 then
               return I;
            end if;
            Max := Shift_Right (Max, 1);
         end loop;
         raise Program_Error;
      end Size;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T)
      is
         Max : constant Long_Long_Long_Unsigned := Norm (T'Last);
         Val : constant Long_Long_Long_Unsigned := Norm (V);
      begin
         Write (Stream, Buffer, Offset, Max, Val);
      end Write;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T)
      is
         Max : constant Long_Long_Long_Unsigned := Norm (T'Last);
         Val : Long_Long_Long_Unsigned;
         R   : T'Base;
      begin
         Read (Stream, Buffer, Offset, Max, Val);
         R := Denorm (Val);
         if R not in T then
            raise Invalid_Value;
         else
            V := R;
         end if;
      end Read;

   end Read_Write_Discrete;

   ------------------------------
   -- Read_Write_Decimal_Fixed --
   ------------------------------

   package body Read_Write_Decimal_Fixed is

      --  To marshal fixed point numbers, go to the internal integer
      --  representation using GNAT specific attributes 'Integer_Value and
      --  'Fixed_Value. Use the longest possible integer in the architecture
      --  as a target (Long_Long_Long_Integer).

      Int_First : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer'Integer_Value (T'First);
      Int_Last  : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer'Integer_Value (T'Last);
      subtype Integer_Value_Type is Long_Long_Long_Integer
      range Int_First .. Int_Last;

      package Impl is new Read_Write_Discrete (Integer_Value_Type);

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T)
      is
      begin
         Impl.Write
           (Stream, Buffer, Offset, Long_Long_Long_Integer'Integer_Value (V));
      end Write;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T)
      is
         R : Long_Long_Long_Integer;
      begin
         Impl.Read (Stream, Buffer, Offset, R);
         if T'Base'Fixed_Value (R) not in T then
            raise Invalid_Value;
         else
            V := T'Base'Fixed_Value (R);
         end if;
      end Read;

   end Read_Write_Decimal_Fixed;

   -------------------------------
   -- Read_Write_Ordinary_Fixed --
   -------------------------------

   package body Read_Write_Ordinary_Fixed is

      --  To marshal fixed point numbers, go to the internal integer
      --  representation using GNAT specific attributes 'Integer_Value and
      --  'Fixed_Value. Use the longest possible integer in the architecture
      --  as a target (Long_Long_Long_Integer).

      Int_First : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer'Integer_Value (T'First);
      Int_Last  : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer'Integer_Value (T'Last);
      subtype Integer_Value_Type is Long_Long_Long_Integer
      range Int_First .. Int_Last;

      package Impl is new Read_Write_Discrete (Integer_Value_Type);

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T)
      is
      begin
         Impl.Write
           (Stream, Buffer, Offset, Long_Long_Long_Integer'Integer_Value (V));
      end Write;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T)
      is
         R : Long_Long_Long_Integer;
      begin
         Impl.Read (Stream, Buffer, Offset, R);
         if T'Base'Fixed_Value (R) not in T then
            raise Invalid_Value;
         else
            V := T'Base'Fixed_Value (R);
         end if;
      end Read;

   end Read_Write_Ordinary_Fixed;

   ----------------------
   -- Read_Write_Float --
   ----------------------

   package body Read_Write_Float is

      --  To marshal floating point numbers, go to standard single, double, or
      --  extended precision types depending on the value of T'Machine_Mantissa
      --  and then use unchecked conversions to get the bitwise representation.
      --  The first step is used to normalize floating point numbers with a
      --  custom size or alignment.

      type Precision is (Single, Double, Extended);

      function Get_Precision return Precision is
        (case T'Machine_Mantissa is
            when 24     => Single,
            when 53     => Double,
            when 64     => Extended,
            when others => raise Program_Error);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : T)
      is
      begin
         case Get_Precision is
            when Single =>
               declare
                  function To_Bits is new Ada.Unchecked_Conversion
                    (Source => Float, Target => Unsigned_32);
                  Max  : constant Long_Long_Long_Unsigned :=
                    Long_Long_Long_Unsigned (Unsigned_32'Last);
                  V_F  : constant Float := Float (V);
                  Bits : constant Unsigned_32 := To_Bits (V_F);
               begin
                  Write (Stream, Buffer, Offset, Max,
                         Long_Long_Long_Unsigned (Bits));
               end;

            when Double =>
               declare
                  function To_Bits is new Ada.Unchecked_Conversion
                    (Source => Long_Float, Target => Unsigned_64);
                  Max  : constant Long_Long_Long_Unsigned :=
                    Long_Long_Long_Unsigned (Unsigned_64'Last);
                  V_F  : constant Long_Float := Long_Float (V);
                  Bits : constant Unsigned_64 := To_Bits (V_F);
               begin
                  Write (Stream, Buffer, Offset, Max,
                         Long_Long_Long_Unsigned (Bits));
               end;

            when Extended =>
               declare
                  function To_Bits is new Ada.Unchecked_Conversion
                    (Source => Long_Long_Float, Target => Unsigned_128);
                  Max  : constant Long_Long_Long_Unsigned :=
                    Long_Long_Long_Unsigned (Unsigned_128'Last);
                  V_F  : constant Long_Long_Float := Long_Long_Float (V);
                  Bits : constant Unsigned_128 := To_Bits (V_F);
               begin
                  Write (Stream, Buffer, Offset, Max,
                         Long_Long_Long_Unsigned (Bits));
               end;
         end case;
      end Write;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Buffer : in out Unsigned_8;
         Offset : in out Offset_Type;
         V      : out T)
      is
      begin
         case Get_Precision is
            when Single =>
               declare
                  function From_Bits is new Ada.Unchecked_Conversion
                    (Source => Unsigned_32, Target => Float);
                  Max  : constant Long_Long_Long_Unsigned :=
                    Long_Long_Long_Unsigned (Unsigned_32'Last);
                  Bits : Long_Long_Long_Unsigned;
                  V_F  : Float;
               begin
                  Read (Stream, Buffer, Offset, Max, Bits);
                  V_F := From_Bits (Unsigned_32 (Bits));
                  if not V_F'Valid or else T'Base (V_F) not in T then
                     raise Invalid_Value;
                  else
                     V := T'Base (V_F);
                  end if;
               end;

            when Double =>
               declare
                  function From_Bits is new Ada.Unchecked_Conversion
                    (Source => Unsigned_64, Target => Long_Float);
                  Max  : constant Long_Long_Long_Unsigned :=
                    Long_Long_Long_Unsigned (Unsigned_64'Last);
                  Bits : Long_Long_Long_Unsigned;
                  V_F  : Long_Float;
               begin
                  Read (Stream, Buffer, Offset, Max, Bits);
                  V_F := From_Bits (Unsigned_64 (Bits));
                  if not V_F'Valid or else T'Base (V_F) not in T then
                     raise Invalid_Value;
                  else
                     V := T'Base (V_F);
                  end if;
               end;

            when Extended =>
               declare
                  function From_Bits is new Ada.Unchecked_Conversion
                    (Source => Unsigned_128, Target => Long_Long_Float);
                  Max  : constant Long_Long_Long_Unsigned :=
                    Long_Long_Long_Unsigned (Unsigned_128'Last);
                  Bits : Long_Long_Long_Unsigned;
                  V_F  : Long_Long_Float;
               begin
                  Read (Stream, Buffer, Offset, Max, Bits);
                  V_F := From_Bits (Unsigned_128 (Bits));
                  if not V_F'Valid or else T'Base (V_F) not in T then
                     raise Invalid_Value;
                  else
                     V := T'Base (V_F);
                  end if;
               end;
         end case;
      end Read;

   end Read_Write_Float;

   ------------
   -- In_Out --
   ------------

   package body In_Out is

      -----------
      -- Input --
      -----------

      procedure Input
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         V      : out T)
      is
         Buffer : Unsigned_8 := 0;
         Offset : Offset_Type := 0;
      begin
         Read (Stream, Buffer, Offset, V);
      end Input;

      ------------
      -- Output --
      ------------

      procedure Output
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         V      : T)
      is
         Buffer : Unsigned_8 := 0;
         Offset : Offset_Type := 0;
      begin
         Write (Stream, Buffer, Offset, V);
         if Offset /= 0 then
            Unsigned_8'Write (Stream, Buffer);
         end if;
      end Output;

   end In_Out;

end TAGAda_Marshalling_Lib;

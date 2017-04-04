-- C332001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the static expression given for a number declaration may be
--      of any numeric type.  Check that the type of a named number is
--      universal_integer or universal_real regardless of the type of the
--      static expression that provides its value.
--
-- TEST DESCRIPTION:
--      This test defines a large cross section of mixed type named numbers.
--      Well, obviously the named numbers don't have types (other than
--      universal_integer and universal_real) associated with them.
--      This test uses typed static values in the definition of several named
--      numbers, and then mixes the named numbers to ensure that their typed
--      origins do not interfere with the use of their values.
--
--
-- CHANGE HISTORY:
--      10 OCT 95   SAIC   Initial version
--      11 APR 96   SAIC   Fixed a few arithmetic errors for 2.1
--      24 NOV 98   RLB    Removed decimal types to insure that this
--                         test is applicable to all implementations.
--
--!

----------------------------------------------------------------- C332001_0

package C332001_0 is

   type Enumeration_Type is (Ah, Gnome, Er, Ay, Shun);

   type Integer_Type is range 0 .. 1_023;

   type Modular_Type is mod 256;

   type Floating_Type is digits 4;

   type Fixed_Type is delta 0.125 range -10.0 .. 10.0;

   type Mod_Array is array (Modular_Type) of Floating_Type;

   type Int_Array is array (Integer_Type) of Fixed_Type;

   type Record_Type is record
      Pinkie : Integer_Type;
      Ring   : Modular_Type;
      Middle : Floating_Type;
      Index  : Fixed_Type;
   end record;

   Mod_Array_Object : Mod_Array;
   Int_Array_Object : Int_Array;

   Record_Object : Record_Type;

   -- numeric_literals

   Nothing_New_Integer : constant := 1;
   Nothing_New_Real    : constant := 1.0;

   -- static constants

   Integ : constant Integer_Type  := 2;
   Modul : constant Modular_Type  := 2;
   Float : constant Floating_Type := 2.0;   -- bad practice, good test
   Fixed : constant Fixed_Type    := 2.0;

   Named_Integer : constant := Integ; -- 2
   Named_Modular : constant := Modul; -- 2
   Named_Float   : constant := Float; -- 2.0
   Named_Fixed   : constant := Fixed; -- 2.0

   -- function calls
   -- parenthetical expressions

   Fn_Integer : constant := Integer_Type'Min (Integ * 2, 8);    -- 4
   Fn_Modular : constant :=
     Modular_Type'Max (Modul + 2, Modular_Type'First);--4
   Fn_Float : constant := (Float**2);                      -- 4.0
   Fn_Fixed : constant := -Fixed;                           -- -2.0
   -- attributes

   Itf : constant := Integer_Type'First;         --   0
   Mtl : constant := Modular_Type'Last;          -- 255
   Mtm : constant := Modular_Type'Modulus;       -- 256
   Enp : constant := Enumeration_Type'Pos (Ay);   --   3
   Mtp : constant := Modular_Type'Pred (Modul);   --   1
   Fts : constant := Fixed_Type'Size;            --   # impdef
   Its : constant := Integer_Type'Succ (Integ);   --   3

   -- array attributes 'First, 'Last, 'Length

   Mafirst : constant := Mod_Array_Object'First;  --    0
   Ialast  : constant := Int_Array_Object'Last;   -- 1023
   Mal     : constant := Mod_Array_Object'Length; --  255
   Ial     : constant := Int_Array_Object'Length; -- 1024

   -- type conversions
   --
   -- F\T Int Mod Flt Fix
   -- Int  .   X   O   X
   -- Mod  O   .   X   O
   -- Flt  X   O   .   X
   -- Fix  O   X   O   .

   Int2mod : constant := Modular_Type (Integ);  -- 2
   Int2fix : constant := Fixed_Type (Integ);    -- 2.0
   Mod2flt : constant := Floating_Type (Modul); -- 2.0
   Flt2int : constant := Integer_Type (Float);   -- 2
   Flt2fix : constant := Fixed_Type (Float);    -- 2.0
   Fix2mod : constant := Modular_Type (Fixed);  -- 2

   procedure Check_Values;

   -- TRANSITION CHECKS
   --
   -- The following were illegal in Ada83; they are now legal in Ada95
   --

   Int_Base_First : constant := Integer'Base'First;  -- # impdef
   Int_First      : constant := Integer'First;       -- # impdef
   Int_Last       : constant := Integer'Last;        -- # impdef
   Int_Val        : constant := Integer'Val (17);     -- 17

   -- END OF TRANSITION CHECKS

end C332001_0;

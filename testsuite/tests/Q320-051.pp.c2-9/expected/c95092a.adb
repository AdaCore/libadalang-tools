-- C95092A.ADA

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
-- OBJECTIVE:
--     CHECK THAT FOR ENTRIES OF TASKS, DEFAULT VALUES OF ALL TYPES CAN
--     BE GIVEN FOR A FORMAL PARAMETER.

-- HISTORY:
--     DHH 03/22/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C95092a is

   subtype Int is Integer range 1 .. 10;
   type Flt is digits 5;
   type Fix is delta 0.125 range 0.0 .. 10.0;
   type Enum is (Red, Blue, Yellow);
   subtype Char is Character range 'A' .. 'F';
   type Arr is array (1 .. 3) of Integer;
   type Rec is record
      A : Int;
      B : Enum;
      C : Char;
   end record;

   function Ident_Flt (E : Flt) return Flt is
   begin
      if Equal (3, 3) then
         return E;
      else
         return 0.0;
      end if;
   end Ident_Flt;

   function Ident_Fix (E : Fix) return Fix is
   begin
      if Equal (3, 3) then
         return E;
      else
         return 0.0;
      end if;
   end Ident_Fix;

   function Ident_Enum (E : Enum) return Enum is
   begin
      if Equal (3, 3) then
         return E;
      else
         return Yellow;
      end if;
   end Ident_Enum;

   function Ident_Char (E : Char) return Char is
   begin
      if Equal (3, 3) then
         return E;
      else
         return 'F';
      end if;
   end Ident_Char;

   function Ident_Arr (E : Arr) return Arr is
      Z : Arr := (3, 2, 1);
   begin
      if Equal (3, 3) then
         return E;
      else
         return Z;
      end if;
   end Ident_Arr;

   function Ident_Rec (E : Rec) return Rec is
      Z : Rec := (10, Yellow, 'F');
   begin
      if Equal (3, 3) then
         return E;
      else
         return Z;
      end if;
   end Ident_Rec;

   task Test_Defaults is
      entry Bool (G : Boolean := True);
      entry Integr (X : in Int := 5);
      entry Float (Y : in Flt := 1.25);
      entry Fixed (Z : in Fix := 1.0);
      entry Enumerat (A : in Enum := Red);
      entry Charactr (B : in Char := 'A');
      entry Arry (C : in Arr := (1, 2, 3));
      entry Recd (D : in Rec := (5, Red, 'A'));
   end Test_Defaults;

   task body Test_Defaults is
   begin

      accept Bool (G : Boolean := True) do
         if G /= Ident_Bool (True) then
            Failed ("BOOLEAN DEFAULT FAILED");
         end if;
      end Bool;

      accept Integr (X : in Int := 5) do
         if X /= Ident_Int (5) then
            Failed ("INTEGER DEFAULT FAILED");
         end if;
      end Integr;

      accept Float (Y : in Flt := 1.25) do
         if Y /= Ident_Flt (1.25) then
            Failed ("FLOAT DEFAULT FAILED");
         end if;
      end Float;

      accept Fixed (Z : in Fix := 1.0) do
         if Z /= Ident_Fix (1.0) then
            Failed ("FIXED DEFAULT FAILED");
         end if;
      end Fixed;

      accept Enumerat (A : in Enum := Red) do
         if A /= Ident_Enum (Red) then
            Failed ("ENUMERATION DEFAULT FAILED");
         end if;
      end Enumerat;

      accept Charactr (B : in Char := 'A') do
         if B /= Ident_Char ('A') then
            Failed ("CHARACTER DEFAULT FAILED");
         end if;
      end Charactr;

      accept Arry (C : in Arr := (1, 2, 3)) do
         for I in 1 .. 3 loop
            if C (I) /= Ident_Int (I) then
               Failed ("ARRAY " & Integer'Image (I) & "DEFAULT FAILED");
            end if;
         end loop;
      end Arry;

      accept Recd (D : in Rec := (5, Red, 'A')) do
         if D.A /= Ident_Int (5) then
            Failed ("RECORD INTEGER DEFAULT FAILED");
         end if;
         if D.B /= Ident_Enum (Red) then
            Failed ("RECORD ENUMERATION DEFAULT FAILED");
         end if;
         if D.C /= Ident_Char ('A') then
            Failed ("RECORD CHARACTER DEFAULT FAILED");
         end if;
      end Recd;

   end Test_Defaults;

begin

   Test
     ("C95092A",
      "CHECK THAT FOR ENTRIES OF TASKS, DEFAULT " &
      "VALUES OF ALL TYPES CAN BE GIVEN FOR A FORMAL " &
      "PARAMETER");

   Test_Defaults.Bool;
   Test_Defaults.Integr;
   Test_Defaults.Float;
   Test_Defaults.Fixed;
   Test_Defaults.Enumerat;
   Test_Defaults.Charactr;
   Test_Defaults.Arry;
   Test_Defaults.Recd;

   Result;
end C95092a;

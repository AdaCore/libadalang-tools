-- C37009A.ADA

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
--     CHECK THAT AN UNCONSTRAINED RECORD TYPE CAN BE USED TO DECLARE A
--     RECORD COMPONENT THAT CAN BE INITIALIZED WITH AN APPROPRIATE
--     EXPLICIT OR DEFAULT VALUE.

-- HISTORY:
--     DHH 02/01/88 CREATED ORIGINAL TEST.

with Report; use Report;

procedure C37009a is

   type Float is digits 5;
   type Color is (Red, Yellow, Blue);

   type Component is record
      I     : Integer := 1;
      X     : Float   := 3.5;
      Bol   : Boolean := False;
      First : Color   := Red;
   end record;
   type Comp_Dis (A : Integer := 1) is record
      I     : Integer := 1;
      X     : Float   := 3.5;
      Bol   : Boolean := False;
      First : Color   := Red;
   end record;
   subtype Smal_Integer is Integer range 1 .. 10;
   type List is array (Integer range <>) of Float;

   type Discrim (P : Smal_Integer := 2) is record
      A : List (1 .. P) := (1 .. P => 1.25);
   end record;

   type Rec_T is                                     -- EXPLICIT INIT.
   record
      T : Component    := (5, 6.0, True, Yellow);
      U : Discrim (3)  := (3, (1 .. 3 => 2.25));
      L : Comp_Dis (5) :=
        (A => 5, I => 5, X => 6.0, Bol => True, First => Yellow);
   end record;

   type Rec_Def_T is                                 -- DEFAULT INIT.
   record
      T : Component;
      U : Discrim;
      L : Comp_Dis;
   end record;

   Rec     : Rec_T;
   Rec_Def : Rec_Def_T;

   function Ident_Flt (X : Float) return Float is
   begin
      if Equal (3, 3) then
         return X;
      else
         return 0.0;
      end if;
   end Ident_Flt;

   function Ident_Enum (X : Color) return Color is
   begin
      if Equal (3, 3) then
         return X;
      else
         return Blue;
      end if;
   end Ident_Enum;

begin
   Test
     ("C37009A",
      "CHECK THAT AN UNCONSTRAINED RECORD TYPE CAN " &
      "BE USED TO DECLARE A RECORD COMPONENT THAT " &
      "CAN BE INITIALIZED WITH AN APPROPRIATE " &
      "EXPLICIT OR DEFAULT VALUE");

   if Rec_Def.T.I /= Ident_Int (1) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF INTEGER");
   end if;

   if Ident_Bool (Rec_Def.T.Bol) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF BOOLEAN");
   end if;

   if Rec_Def.T.X /= Ident_Flt (3.5) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF REAL");
   end if;

   if Rec_Def.T.First /= Ident_Enum (Red) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF ENUMERATION");
   end if;

   for I in 1 .. 2 loop
      if Rec_Def.U.A (I) /= Ident_Flt (1.25) then
         Failed
           ("INCORRECT DEFAULT INITIALIZATION OF ARRAY " &
            "POSITION " &
            Integer'Image (I));
      end if;
   end loop;

   if Rec_Def.L.A /= Ident_Int (1) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF DISCRIMINANT " & "- L");
   end if;

   if Rec_Def.L.I /= Ident_Int (1) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF INTEGER - L");
   end if;

   if Ident_Bool (Rec_Def.L.Bol) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF BOOLEAN - L");
   end if;

   if Rec_Def.L.X /= Ident_Flt (3.5) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF REAL - L");
   end if;

   if Rec_Def.L.First /= Ident_Enum (Red) then
      Failed ("INCORRECT DEFAULT INITIALIZATION OF ENUMERATION - L");
   end if;
--------------------------------------------------------------------
   if Rec.T.I /= Ident_Int (5) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF INTEGER");
   end if;

   if not Ident_Bool (Rec.T.Bol) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF BOOLEAN");
   end if;

   if Rec.T.X /= Ident_Flt (6.0) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF REAL");
   end if;

   if Rec.T.First /= Yellow then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF ENUMERATION");
   end if;

   for I in 1 .. 3 loop
      if Rec.U.A (I) /= Ident_Flt (2.25) then
         Failed
           ("INCORRECT EXPLICIT INITIALIZATION OF ARRAY " &
            "POSITION " &
            Integer'Image (I));
      end if;
   end loop;

   if Rec.L.A /= Ident_Int (5) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF DISCRIMINANT " & "- L");
   end if;

   if Rec.L.I /= Ident_Int (5) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF INTEGER - L");
   end if;

   if not Ident_Bool (Rec.L.Bol) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF BOOLEAN - L");
   end if;

   if Rec.L.X /= Ident_Flt (6.0) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF REAL - L");
   end if;

   if Rec.L.First /= Ident_Enum (Yellow) then
      Failed ("INCORRECT EXPLICIT INITIALIZATION OF ENUMERATION " & "- L");
   end if;

   Result;

end C37009a;

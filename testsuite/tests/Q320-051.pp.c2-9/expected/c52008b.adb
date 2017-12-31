-- C52008B.ADA

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
--     CHECK THAT A RECORD VARIABLE DECLARED WITH A SPECIFIED
--     DISCRIMINANT CONSTRAINT CANNOT HAVE A DISCRIMINANT VALUE ALTERED
--     BY ASSIGNMENT.  ASSIGNING AN ENTIRE RECORD VALUE WITH A
--     DIFFERENT DISCRIMINANT VALUE SHOULD RAISE CONSTRAINT_ERROR AND
--     LEAVE THE TARGET VARIABLE UNALTERED.  THIS TEST USES NON-STATIC
--     DISCRIMINANT VALUES.

-- HISTORY:
--     ASL  6/25/81  CREATED ORIGINAL TEST
--     JRK 11/18/82
--     RJW  8/17/89  ADDED SUBTYPE 'SUBINT'.

with Report;
procedure C52008b is

   use Report;

   type Rec1 (D1, D2 : Integer) is record
      Comp1 : String (D1 .. D2);
   end record;

   type Ar_Rec1 is
     array (Natural range <>) of Rec1 (Ident_Int (3), Ident_Int (5));

   subtype Subint is Integer range -128 .. 127;

   type Rec2 (D1, D2, D3, D4 : Subint := 0) is record
      Comp1 : String (1 .. D1);
      Comp2 : String (D2 .. D3);
      Comp5 : Ar_Rec1 (1 .. D4);
      Comp6 : Rec1 (D3, D4);
   end record;

   Str : String (Ident_Int (3) .. Ident_Int (5)) := "ZZZ";

   R1a : Rec1 (Ident_Int (3), Ident_Int (5)) := (3, 5, Str);
   R1c : Rec1 (5, 6) := (5, 6, Comp1 => (5 .. 6 => 'K'));

   Q, R : Rec2 (Ident_Int (2), Ident_Int (3), Ident_Int (5), Ident_Int (6));
   Temp : Rec2 (2, 3, 5, 6);

   W  : Rec2 (1, 4, 6, 8);
   Ok : Boolean := False;

begin

   Test
     ("C52008B",
      "CANNOT ASSIGN RECORD VARIABLE WITH SPECIFIED " &
      "DISCRIMINANT VALUE A VALUE WITH A DIFFERENT " &
      "(DYNAMIC) DISCRIMINANT VALUE");

   begin
      R1a := (Ident_Int (3), 5, "XYZ");

      R :=
        (Ident_Int (2), Ident_Int (3), Ident_Int (5), Ident_Int (6), "AB", Str,
         (1 .. 6 => R1a), R1c);

      Temp    := R;
      Q       := Temp;
      R.Comp1 := "YY";
      Ok      := True;
      W       := R;
      Failed ("ASSIGNMENT MADE USING INCORRECT DISCRIMINANT " & "VALUES");
   exception
      when Constraint_Error =>
         if not Ok or Q /= Temp or R = Temp or R = Q or W.D4 /= 8 then
            Failed ("LEGITIMATE ASSIGNMENT FAILED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION");
   end;

   Result;

end C52008b;

-- C38104A.ADA

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
--     CHECK THAT AN INCOMPLETE TYPE WITH DISCRIMINANTS CAN BE
--     USED IN AN ACCESS TYPE DEFINITION WITH A COMPATIBLE DISCRIMINANT
--     CONSTRAINT.

-- HISTORY:
--     PMW 09/01/88  CREATED ORIGINAL TEST BY RENAMING E38104A.ADA.

with Report; use Report;
procedure C38104a is

begin

   Test
     ("C38104A",
      "INCOMPLETELY DECLARED TYPE CAN BE USED AS TYPE " &
      "MARK IN ACCESS TYPE DEFINITION, AND CAN BE CONSTRAINED " &
      "THERE OR LATER IF INCOMPLETE TYPE HAD DISCRIMINANT(S)");

   declare
      type T1;
      type T1_Name is access T1;

      type T1 is record
         Comp : Integer;
      end record;

      type T2 (Disc : Integer := 5);
      type T2_Name1 is access T2 (5);
      type T2_Name2 is access T2;

      subtype Sub_T2_Name2 is T2_Name2 (5);
      type T2_Name2_Name is access T2_Name2 (5);
      X : T2_Name2 (5);

      type T2 (Disc : Integer := 5) is record
         Comp : T2_Name2 (Disc);
      end record;

      X1n      : T1_Name;
      X2a, X2b : T2;
      X2n2     : T2_Name2;

   begin
      if Equal (3, 3) then
         X1n := new T1'(Comp => 5);
      end if;

      if X1n.Comp /= 5 then
         Failed ("ASSIGNMENT FAILED - 1");
      end if;

      X2a      := (Disc => Ident_Int (7), Comp => null);
      X2n2     := new T2 (Ident_Int (7));
      X2n2.all := X2a;

      if Equal (3, 3) then
         X2b := (Disc => Ident_Int (7), Comp => X2n2);
      end if;

      if X2b.Comp.Comp /= null or X2b.Comp.Disc /= 7 then
         Failed ("ASSIGNMENT FAILED - 2");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;

end C38104a;

-- C85005G.ADA

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
--     CHECK THAT ANY SUBTYPE CONSTRAINT IMPOSED BY THE TYPE MARK USED
--     IN THE RENAMING DECLARATION IS IGNORED, AND THE SUBTYPE
--     CONSTRAINT ASSOCIATED WITH THE RENAMED VARIABLE IS USED INSTEAD.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     JET 07/26/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report; use Report;
procedure C85005g is

   subtype Int is Integer range -100 .. 100;

   I : Integer := Ident_Int (Integer'Last);
   J : Int     := Ident_Int (Int'Last);

   Dg1 : Integer := Ident_Int (Integer'Last);
   Dg2 : Int     := Ident_Int (Int'Last);

   Xi : Int renames I;
   Xj : Integer renames J;

   generic
      G1 : in out Int;
      G2 : in out Integer;
   procedure Gen;

   procedure Gen is
      Xg1 : Int renames G1;
      Xg2 : Integer renames G2;
   begin
      if Xg1 /= Integer'Last then
         Failed ("INCORRECT VALUE OF RENAMING VARIABLE - G1");
      end if;

      Xg1 := Ident_Int (Integer'First);

      if Xg1 /= Integer'First then
         Failed ("INCORRECT VALUE OF RENAMING VARIABLE - G2");
      end if;

      if Xg2 /= Int'Last then
         Failed ("INCORRECT VALUE OF RENAMING VARIABLE - G3");
      end if;

      Xg2 := Ident_Int (Int'First);

      if Xg2 /= Int'First then
         Failed ("INCORRECT VALUE OF RENAMING VARIABLE - G4");
      end if;

      begin
         Xg2 := Ident_Int (Integer'Last);
         Failed ("NO EXCEPTION RAISED BY XG2 := INTEGER'LAST");
         if not Equal (Xg2, Xg2) then
            Comment ("DON'T OPTIMIZE XG2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION (G)");
      end;
   end Gen;

   procedure Proc is new Gen (Dg1, Dg2);

begin
   Test
     ("C85005G",
      "CHECK THAT ANY SUBTYPE CONSTRAINT IMPOSED BY " &
      "THE TYPE MARK USED IN THE RENAMING " &
      "DECLARATION IS IGNORED, AND THE SUBTYPE " &
      "CONSTRAINT ASSOCIATED WITH THE RENAMED " &
      "VARIABLE IS USED INSTEAD");

   if Xi /= Integer'Last then
      Failed ("INCORRECT VALUE OF RENAMING VARIABLE - 1");
   end if;

   Xi := Ident_Int (Integer'First);

   if Xi /= Integer'First then
      Failed ("INCORRECT VALUE OF RENAMING VARIABLE - 2");
   end if;

   if Xj /= Int'Last then
      Failed ("INCORRECT VALUE OF RENAMING VARIABLE - 3");
   end if;

   Xj := Ident_Int (Int'First);

   if Xj /= Int'First then
      Failed ("INCORRECT VALUE OF RENAMING VARIABLE - 4");
   end if;

   begin
      Xj := Ident_Int (Integer'Last);
      Failed ("NO EXCEPTION RAISED BY XJ := INTEGER'LAST");
      if not Equal (Xj, Xj) then
         Comment ("DON'T OPTIMIZE XJ");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION - 1");
   end;

   Proc;

   Result;
exception
   when others =>
      Failed ("UNEXPECTED EXCEPTION - 2");
      Result;
end C85005g;

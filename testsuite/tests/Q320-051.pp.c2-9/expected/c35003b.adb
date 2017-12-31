-- C35003B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A SUBTYPE INDICATION
--     OF A DISCRETE GENERIC FORMAL TYPE WHEN THE LOWER OR UPPER BOUND
--     OF A NON-NULL RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     JET 07/08/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C35003b is

   type Enum is (We, Love, Writing, Tests);
   type Int is range -10 .. 10;

   generic
      type Gen_Enum is (<>);
      type Gen_Int is range <>;
   package Gen_Pack is
      subtype Subenum is
        Gen_Enum range Gen_Enum'Succ (Gen_Enum'First) ..
            Gen_Enum'Pred (Gen_Enum'Last);
      subtype Subint is
        Gen_Int range Gen_Int'Succ (Gen_Int'First) ..
            Gen_Int'Pred (Gen_Int'Last);
      type A1 is array (0 .. Gen_Int'Last) of Integer;
      type A2 is array (Gen_Int range Gen_Int'First .. 0) of Integer;
   end Gen_Pack;

   package body Gen_Pack is
   begin
      Test
        ("C35003B",
         "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
         "FOR A SUBTYPE INDICATION OF A DISCRETE " &
         "GENERIC FORMAL TYPE WHEN THE LOWER OR " &
         "UPPER BOUND OF A NON-NULL RANGE LIES " &
         "OUTSIDE THE RANGE OF THE TYPE MARK");
      begin
         declare
            subtype Subsubenum is Subenum range Gen_Enum'First .. Subenum'Last;
         begin
            Failed ("NO EXCEPTION RAISED (E1)");
            declare
               Z : Subsubenum := Subenum'First;
            begin
               if not Equal (Subsubenum'Pos (Z), Subsubenum'Pos (Z)) then
                  Comment ("DON'T OPTIMIZE Z");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG " & "PLACE (E1)");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (E1)");
      end;

      begin
         declare
            type A is
              array (Subenum range Subenum'First .. Gen_Enum'Last) of Integer;
         begin
            Failed ("NO EXCEPTION RAISED (E2)");
            declare
               Z : A := (others => 0);
            begin
               if not Equal (Z (Subenum'First), Z (Subenum'First)) then
                  Comment ("DON'T OPTIMIZE Z");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG PLACE " & "(E2)");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (E2)");
      end;

      begin
         declare
            type I is access Subint range Gen_Int'First .. Subint'Last;
         begin
            Failed ("NO EXCEPTION RAISED (I1)");
            declare
               Z : I := new Subint'(Subint'First);
            begin
               if not Equal (Integer (Z.all), Integer (Z.all)) then
                  Comment ("DON'T OPTIMIZE Z");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG PLACE " & "(I1)");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (I1)");
      end;

      begin
         declare
            type I is new Subint range Subint'First .. Gen_Int'Last;
         begin
            Failed ("NO EXCEPTION RAISED (I2)");
            declare
               Z : I := I'First;
            begin
               if not Equal (Integer (Z), Integer (Z)) then
                  Comment ("DON'T OPTIMIZE Z");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG PLACE " & "(I2)");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (I2)");
      end;

      begin
         declare
            subtype I is Subint range A1'Range;
         begin
            Failed ("NO EXCEPTION RAISED (R1)");
            declare
               Z : I := Subint'First;
            begin
               if not Equal (Integer (Z), Integer (Z)) then
                  Comment ("DON'T OPTIMIZE Z");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG PLACE " & "(R1)");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (R1)");
      end;

      begin
         declare
            subtype I is Subint range A2'Range;
         begin
            Failed ("NO EXCEPTION RAISED (R2)");
            declare
               Z : I := 1;
            begin
               if not Equal (Integer (Z), Integer (Z)) then
                  Comment ("DON'T OPTIMIZE Z");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG PLACE " & "(R2)");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (R2)");
      end;
   end Gen_Pack;

   package Enum_Pack is new Gen_Pack (Enum, Int);

begin
   Result;
end C35003b;

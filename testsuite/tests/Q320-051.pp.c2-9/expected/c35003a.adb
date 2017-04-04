-- C35003A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR AN INTEGER OR
--     ENUMERATION SUBTYPE INDICATION WHEN THE LOWER OR UPPER BOUND
--     OF A NON-NULL RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     JET 01/25/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C35003a is

   type Enum is (Zero, One, Two, Three);
   subtype Subenum is Enum range One .. Two;
   type Int is range 1 .. 10;
   subtype Subint is Integer range -10 .. 10;
   type A1 is array (0 .. 11) of Integer;
   type A2 is array (Integer range -11 .. 10) of Integer;

begin
   Test
     ("C35003A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR AN " &
      "INTEGER OR ENUMERATION SUBTYPE INDICATION " &
      "WHEN THE LOWER OR UPPER BOUND OF A NON-NULL " &
      "RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK");
   begin
      declare
         subtype Subsubenum is Subenum range Zero .. Two;
      begin
         Failed ("NO EXCEPTION RAISED (E1)");
         declare
            Z : Subsubenum := One;
         begin
            if not Equal (Subsubenum'Pos (Z), Subsubenum'Pos (Z)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (E1)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (E1)");
   end;

   begin
      declare
         type A is array (Subenum range One .. Three) of Integer;
      begin
         Failed ("NO EXCEPTION RAISED (E2)");
         declare
            Z : A := (others => 0);
         begin
            if not Equal (Z (One), Z (One)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (E2)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (E2)");
   end;

   begin
      declare
         type I is access Int range Int (Ident_Int (0)) .. 10;
      begin
         Failed ("NO EXCEPTION RAISED (I1)");
         declare
            Z : I := new Int'(1);
         begin
            if not Equal (Integer (Z.all), Integer (Z.all)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (I1)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (I1)");
   end;

   begin
      declare
         type I is new Int range 1 .. Int'Succ (10);
      begin
         Failed ("NO EXCEPTION RAISED (I2)");
         declare
            Z : I := 1;
         begin
            if not Equal (Integer (Z), Integer (Z)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (I2)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (I2)");
   end;

   begin
      declare
         type R is record
            A : Subint range Ident_Int (-11) .. 0;
         end record;
      begin
         Failed ("NO EXCEPTION RAISED (S1)");
         declare
            Z : R := (A => 1);
         begin
            if not Equal (Integer (Z.A), Integer (Z.A)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (S1)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (S1)");
   end;

   begin
      declare
         Z : Subint range 0 .. Ident_Int (11) := 0;
      begin
         Failed ("NO EXCEPTION RAISED (S2)");
         if not Equal (Integer (Z), Integer (Z)) then
            Comment ("DON'T OPTIMIZE Z");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (S2)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (S2)");
   end;

   begin
      declare
         subtype I is Subint range A1'Range;
      begin
         Failed ("NO EXCEPTION RAISED (R1)");
         declare
            Z : I := 1;
         begin
            if not Equal (Integer (Z), Integer (Z)) then
               Comment ("DON'T OPTIMIZE Z");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN WRONG PLACE (R1)");
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
            Failed ("EXCEPTION RAISED IN WRONG PLACE (R2)");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED (R2)");
   end;

   Result;

end C35003a;

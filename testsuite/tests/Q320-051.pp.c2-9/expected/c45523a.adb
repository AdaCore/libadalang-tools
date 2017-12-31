-- C45523A.ADA

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
--     FOR FLOATING POINT TYPES, IF MACHINE_OVERFLOWS IS TRUE AND
--     EITHER THE RESULT OF MULTIPLICATION LIES OUTSIDE THE RANGE OF THE
--     BASE TYPE, OR AN ATTEMPT IS MADE TO DIVIDE BY ZERO, THEN
--     CONSTRAINT_ERROR IS RAISED.  THIS TESTS
--     DIGITS 5.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- HISTORY:
--     BCB 02/09/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY
--     KAS 11/14/95  DELETED USAGE OF 'SAFE_LARGE
--     KAS 11/30/95  GOT IT RIGHT THIS TIME

with Report; use Report;

procedure C45523a is

   type Flt is digits 5;

   F : Flt;

   function Ident_Flt (X : Flt) return Flt is
   begin
      if Equal (3, 3) then
         return X;
      else
         return 0.0;
      end if;
   end Ident_Flt;

   function Equal_Flt (One, Two : Flt) return Boolean is
   begin
      return One = Two * Flt (Ident_Int (1));
   end Equal_Flt;

begin
   Test
     ("C45523A",
      "FOR FLOATING POINT TYPES, IF MACHINE_" &
      "OVERFLOWS IS TRUE AND EITHER THE RESULT OF " &
      "MULTIPLICATION LIES OUTSIDE THE RANGE OF THE " &
      "BASE TYPE, OR AN ATTEMPT IS MADE TO DIVIDE BY " &
      "ZERO, THEN CONSTRAINT_ERROR IS RAISED." & "THIS TESTS DIGITS 5");

   if Flt'Machine_Overflows then
      begin
         F := (Flt'Base'Last) * Ident_Flt (2.0);
         Failed ("CONSTRAINT_ERROR WAS NOT RAISED FOR MULTIPLICATION");
         if Equal_Flt (F, F**Ident_Int (1)) then
            Comment ("DON'T OPTIMIZE F");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR WAS RAISED FOR " & "MULTIPLICATION");
         when others =>
            Failed
              ("AN EXCEPTION OTHER THAN " &
               "CONSTRAINT_ERROR WAS RAISED FOR " & "MULTIPLICATION");
      end;
      begin
         F := (Flt'Last) / Ident_Flt (0.0);
         Failed ("CONSTRAINT_ERROR WAS NOT RAISED FOR DIVISION BY ZERO");
         if Equal_Flt (F, F**Ident_Int (1)) then
            Comment ("DON'T OPTIMIZE F");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR WAS RAISED FOR " & "DIVISION BY ZERO");
         when others =>
            Failed
              ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
               "WAS RAISED FOR DIVISION BY ZERO");
      end;
   else
      Not_Applicable
        ("THIS TEST IS NOT APPLICABLE DUE TO " &
         "MACHINE_OVERFLOWS BEING FALSE");
   end if;

   Result;
end C45523a;

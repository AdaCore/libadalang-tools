-- CB3003A.ADA

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
-- CHECK THAT THE NON-SPECIFIC RAISE STATEMENT PROPAGATES THE EXCEPTION
--    FOR FURTHER PROCESSING(HANDLING) IN ANOTHER HANDLER.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- DCB 04/01/80
-- JRK 11/19/80
-- SPS 11/2/82
-- MRM 03/30/93 REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report;
procedure Cb3003a is

   use Report;

   Flow_Count : Integer := 0;
   E1, E2 : exception;

begin
   Test
     ("CB3003A",
      "CHECK THAT THE NON-SPECIFIC RAISE STATEMENT" &
      " PROPAGATES THE ERROR FOR FURTHER HANDLING IN ANOTHER" &
      " HANDLER");

   -------------------------------------------------------

   begin
      begin
         begin
            Flow_Count := Flow_Count + 1;
            raise E1;
            Failed ("EXCEPTION NOT RAISED (CASE 1)");
         exception
            when others =>
               Flow_Count := Flow_Count + 1;
               raise;
               Failed ("EXCEPTION NOT RERAISED (CASE 1; " & "INNER)");
         end;

      exception
         -- A HANDLER SPECIFIC TO THE RAISED EXCEPTION (E1).
         when E1 =>
            Flow_Count := Flow_Count + 1;
            raise;
            Failed ("EXCEPTION NOT RERAISED (CASE 1; OUTER)");
         when others =>
            Failed ("WRONG EXCEPTION RAISED (CASE 1)");
      end;

   exception
      when E1 =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION PASSED (CASE 1)");
   end;

   -------------------------------------------------------

   begin
      begin
         begin
            Flow_Count := Flow_Count + 1;
            raise E1;
            Failed ("EXCEPTION NOT RAISED (CASE 2)");
         exception
            when others =>
               Flow_Count := Flow_Count + 1;
               raise;
               Failed ("EXCEPTION NOT RERAISED (CASE 2; " & "INNER)");
         end;

      exception
         -- A HANDLER FOR SEVERAL EXCEPTIONS INCLUDING THE ONE RAISED.
         when Constraint_Error =>
            Failed ("WRONG EXCEPTION RAISED (CONSTRAINT_ERROR)");
         when E2 =>
            Failed ("WRONG EXCEPTION RAISED (E2)");
         when Program_Error | E1 | Tasking_Error =>
            Flow_Count := Flow_Count + 1;
            raise;
            Failed ("EXCEPTION NOT RERAISED (CASE 2; OUTER)");
         when Storage_Error =>
            Failed ("WRONG EXCEPTION RAISED (STORAGE_ERROR)");
         when others =>
            Failed ("WRONG EXCEPTION RAISED (OTHERS)");
      end;

   exception
      when E1 =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION PASSED (CASE 2)");
   end;

   -------------------------------------------------------

   begin
      begin
         begin
            Flow_Count := Flow_Count + 1;
            raise E1;
            Failed ("EXCEPTION NOT RAISED (CASE 3)");
         exception
            when others =>
               Flow_Count := Flow_Count + 1;
               raise;
               Failed ("EXCEPTION NOT RERAISED (CASE 3; " & "INNER)");
         end;

      exception
         -- A NON-SPECIFIC HANDLER.
         when Constraint_Error | E2 =>
            Failed ("WRONG EXCEPTION RAISED " & "(CONSTRAINT_ERROR | E2)");
         when others =>
            Flow_Count := Flow_Count + 1;
            raise;
            Failed ("EXCEPTION NOT RERAISED (CASE 3; OUTER)");
      end;

   exception
      when E1 =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION PASSED (CASE 3)");
   end;

   -------------------------------------------------------

   if Flow_Count /= 12 then
      Failed ("INCORRECT FLOW_COUNT VALUE");
   end if;

   Result;
end Cb3003a;

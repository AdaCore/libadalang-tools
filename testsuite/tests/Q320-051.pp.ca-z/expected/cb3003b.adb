-- CB3003B.ADA

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
-- CHECK THAT A NON-EXPLICIT RAISE STATEMENT MAY APPEAR IN A BLOCK
-- STATEMENT WITHIN AN EXCEPTION HANDLER; IF THE BLOCK STATEMENT
-- INCLUDES A HANDLER FOR THE CURRENT EXCEPTION, THEN THE INNER
-- HANDLER RECEIVES CONTROL.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- L.BROWN  10/08/86
-- MRM  03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report; use Report;

procedure Cb3003b is

   My_Error : exception;

begin
   Test
     ("CB3003B",
      "A NON-EXPLICIT RAISE STATEMENT MAY APPEAR IN A " &
      "BLOCK STATEMENT WITHIN AN EXCEPTION HANDLER");

   begin
      begin
         if Equal (3, 3) then
            raise My_Error;
         end if;
         Failed ("MY_ERROR WAS NOT RAISED 1");
      exception
         when My_Error =>
            begin
               if Equal (3, 3) then
                  raise;
               end if;
               Failed ("MY_ERROR WAS NOT RAISED 2");
            exception
               when My_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED 1");
            end;
         when others =>
            Failed ("WRONG EXCEPTION RAISED 2");
      end;
   exception
      when My_Error =>
         Failed ("CONTROL PASSED TO OUTER HANDLER 1");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 1");
   end;

   begin
      begin
         if Equal (3, 3) then
            raise My_Error;
         end if;
         Failed ("MY_ERROR WAS NOT RAISED 3");
      exception
         when Constraint_Error | My_Error | Tasking_Error =>
            begin
               if Equal (3, 3) then
                  raise;
               end if;
               Failed ("MY_ERROR WAS NOT RAISED 4");
            exception
               when My_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED 3");
            end;
         when others =>
            Failed ("WRONG EXCEPTION RAISED 4");
      end;
   exception
      when My_Error =>
         Failed ("CONTROL PASSED TO OUTER HANDLER 2");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 2");
   end;

   begin
      begin
         if Equal (3, 3) then
            raise My_Error;
         end if;
         Failed ("MY_ERROR WAS NOT RAISED 5");
      exception
         when others =>
            begin
               if Equal (3, 3) then
                  raise;
               end if;
               Failed ("MY_ERROR WAS NOT RAISED 6");
            exception
               when My_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED 5");
            end;
      end;
   exception
      when My_Error =>
         Failed ("CONTROL PASSED TO OUTER HANDLER 3");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 3");
   end;

   Result;

end Cb3003b;

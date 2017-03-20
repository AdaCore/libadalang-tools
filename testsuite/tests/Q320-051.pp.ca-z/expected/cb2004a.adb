-- CB2004A.ADA

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
-- CHECK THAT A PREDEFINED OR A PROGRAMMER DEFINED EXCEPTION
--    RAISED SEVERAL LEVELS INSIDE A HIERARCHY OF NESTED BLOCKS
--    CAN BE SUCCESSFULLY HANDLED IN AN OUTER BLOCK.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- DCB 5/12/80
-- JRK 11/17/80
-- SPS 11/2/82
-- MRM 03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report;
procedure Cb2004a is

   use Report;

   Flow_Count : Integer := 0;

   E1, E2, E3 : exception;

begin
   Test
     ("CB2004A",
      "CHECK THAT EXCEPTIONS RAISED INSIDE NESTED " &
      "BLOCKS CAN BE HANDLED IN OUTER BLOCKS");

   begin

      -- PROGRAMMER-DEFINED EXCEPTION, SINGLE EXCEPTON_CHOICE.

      begin
         begin
            begin
               Flow_Count := Flow_Count + 1;
               raise E1;
               Failed ("PROGRAMMER-DEFINED EXCEPTION " & "NOT RAISED  #1");

            exception
               when E2 | E3 =>
                  Failed
                    ("WRONG PROGRAMMER-" & "DEFINED EXCEPTION HANDLED   #1");
            end;

         exception
            when Constraint_Error | Program_Error | Storage_Error | Tasking_Error | E2 | E3 =>
               Failed ("WRONG  " & "EXCEPTION HANDLED   #1");
         end;

      exception
         when E1 =>
            Flow_Count := Flow_Count + 1;
      end;

      -- PROGRAMMER-DEFINED EXCEPTION, MULTIPLE EXCEPTION_CHOICES.

      begin
         begin
            begin
               Flow_Count := Flow_Count + 1;
               raise E2;
               Failed ("PROGRAMMER-DEFINED EXCEPTION " & "NOT RAISED  #2");

            exception
               when E1 | E3 =>
                  Failed
                    ("WRONG PROGRAMMER-" & "DEFINED EXCEPTION HANDLED   #2");
            end;

         exception
            when Constraint_Error | Program_Error | Storage_Error | Tasking_Error | E1 | E3 =>
               Failed ("WRONG  " & "EXCEPTION HANDLED   #2");
         end;

      exception
         when E3 =>
            Failed ("WRONG EXCEPTION HANDLED  #2A");
         when E1 | E2 | Constraint_Error =>
            Flow_Count := Flow_Count + 1;
      end;

      -- PROGRAMMER-DEFINED EXCEPTION, 'OTHERS' CHOICE.

      begin
         begin
            begin
               Flow_Count := Flow_Count + 1;
               raise E1;
               Failed ("PROGRAMMER-DEFINED EXCEPTION " & "NOT RAISED  #3");

            exception
               when E2 | E3 =>
                  Failed
                    ("WRONG PROGRAMMER-" & "DEFINED EXCEPTION HANDLED   #3");
            end;

         exception
            when Constraint_Error | Program_Error | Storage_Error | Tasking_Error | E2 | E3 =>
               Failed ("WRONG " & "EXCEPTION HANDLED   #3");
         end;

      exception
         when E2 | Constraint_Error =>
            Failed ("WRONG EXCEPTION HANDLED  #3A");
         when others =>
            Flow_Count := Flow_Count + 1;
      end;

      -- PREDEFINED EXCEPTION, SINGLE EXCEPTION_CHOICE.

      begin
         begin
            begin
               Flow_Count := Flow_Count + 1;
               raise Constraint_Error;
               Failed ("PREDEFINED EXCEPTION NOT RAISED  #4");

            exception
               when E1 | E2 | E3 =>
                  Failed ("WRONG " & "EXCEPTION HANDLED   #4");
            end;

         exception
            when Program_Error | Storage_Error | Tasking_Error =>
               Failed ("WRONG PREDEFINED " & "EXCEPTION HANDLED   #4");
         end;

      exception
         when Constraint_Error =>
            Flow_Count := Flow_Count + 1;
      end;

      -- PREDEFINED EXCEPTION, MULTIPLE EXCEPTION_CHOICES.

      begin
         begin
            begin
               Flow_Count := Flow_Count + 1;
               raise Constraint_Error;
               Failed ("PREDEFINED EXCEPTION NOT RAISED  #5");

            exception
               when E1 | E2 | E3 =>
                  Failed ("WRONG " & "EXCEPTION HANDLED   #5");
            end;

         exception
            when Program_Error | Storage_Error | Tasking_Error =>
               Failed ("WRONG PREDEFINED " & "EXCEPTION HANDLED   #5");
         end;

      exception
         when E1 | E2 =>
            Failed ("WRONG EXCEPTION HANDLED  #5A");
         when Constraint_Error | E3 =>
            Flow_Count := Flow_Count + 1;
      end;

      -- PREDEFINED EXCEPTION, 'OTHERS' CHOICE.

      begin
         begin
            begin
               Flow_Count := Flow_Count + 1;
               raise Constraint_Error;
               Failed ("PREDEFINED EXCEPTION NOT RAISED  #6");

            exception
               when E1 | E2 | E3 =>
                  Failed ("WRONG " & " EXCEPTION HANDLED   #6");
            end;

         exception
            when Program_Error | Storage_Error | Tasking_Error =>
               Failed ("WRONG PREDEFINED " & "EXCEPTION HANDLED   #6");
         end;

      exception
         when E1 =>
            Failed ("WRONG EXCEPTION HANDLED  #6A");
         when others =>
            Flow_Count := Flow_Count + 1;
      end;

   exception
      when E1 | E2 | E3 =>
         Failed ("PROGRAMMER-DEFINED EXCEPTION HANDLED IN" & "WRONG SCOPE");
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR HANDLED IN WRONG SCOPE");
      when others =>
         Failed ("OTHER EXCEPTIONS HANDLED IN WRONG SCOPE");
   end;

   if Flow_Count /= 12 then
      Failed ("INCORRECT FLOW_COUNT VALUE");
   end if;

   Result;
end Cb2004a;

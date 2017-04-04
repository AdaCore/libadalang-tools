-- CB1001A.ADA

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
-- CHECK THAT ALL PREDEFINED EXCEPTIONS MAY BE RAISED EXPLICITLY
--    AND MAY HAVE HANDLERS WRITTEN FOR THEM.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- DCB 03/25/80
-- JRK 11/17/80
-- SPS 11/2/82
-- MRM 03/30/93 REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report;
procedure Cb1001a is

   use Report;

   Flow_Count : Integer := 0;

begin
   Test
     ("CB1001A",
      "CHECK THAT ALL PREDEFINED EXCEPTIONS MAY BE " &
      "RAISED EXPLICITLY AND MAY HAVE HANDLERS WRITTEN FOR THEM");

   begin
      raise Constraint_Error;
      Failed ("NO EXCEPTION RAISED WHEN CONSTRAINT_ERROR EXPECTED");

   exception
      when Constraint_Error =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED WHEN CONSTRAINT_ERROR " & "EXPECTED");
   end;

   begin
      raise Program_Error;
      Failed ("NO EXCEPTION RAISED WHEN PROGRAM_ERROR EXPECTED");
   exception
      when Program_Error =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED WHEN PROGRAM_ERROR " & "EXPECTED");
   end;

   begin
      raise Storage_Error;
      Failed ("NO EXCEPTION RAISED WHEN STORAGE_ERROR EXPECTED");

   exception
      when Storage_Error =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED WHEN STORAGE_ERROR " & "EXPECTED");
   end;

   begin
      raise Tasking_Error;
      Failed ("NO EXCEPTION RAISED WHEN TASKING_ERROR EXPECTED");

   exception
      when Tasking_Error =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED WHEN TASKING_ERROR " & "EXPECTED");
   end;

   if Flow_Count /= 4 then
      Failed ("WRONG FLOW_COUNT VALUE");
   end if;

   Result;
end Cb1001a;

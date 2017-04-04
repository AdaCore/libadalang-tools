-- C35703A.ADA

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
-- CHECK THAT 'FIRST AND 'LAST EXIST AND CAN BE ASSIGNED.  CHECK THAT
-- 'FIRST IS LESS THAN OR EQUAL TO 'LAST.

-- BAW 5 SEPT 80
-- R.WILLIAMS 8/21/86    ADDED A TYPE DECLARED WITHOUT A RANGE
--                       CONSTRAINT.  RENAMED TO -B.  ADDED EXCEPTION
--                       HANDLERS.
-- GMT 6/29/87           MOVED THE CALL TO  REPORT.TEST INTO A NEWLY
--                       CREATED PACKAGE NAMED SHOW_TEST_HEADER.

with Report; use Report;
procedure C35703a is

   type Real1 is digits 2 range 0.25 .. 0.5;
   type Real2 is digits 3;

   package Show_Test_Header is
   -- PURPOSE OF THIS PACKAGE:
   -- WE WANT THE TEST HEADER INFORMATION TO BE
   -- PRINTED  BEFORE  ANY OF THE  PASS/FAIL  MESSAGES.
   end Show_Test_Header;

   package body Show_Test_Header is
   begin
      Test
        ("C35703A",
         "CHECK THAT FIRST AND LAST CAN BE ASSIGNED " &
         "AND THAT FIRST <= LAST");
   end Show_Test_Header;

   package Xpkg is
      X : Real1;
   end Xpkg;

   package body Xpkg is
   begin
      X := Real1'First;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " & "REAL1'FIRST");
      when others =>
         Failed ("OTHER EXCEPTION RAISED BY ASSIGNMENT OF " & "REAL1'FIRST");
   end Xpkg;

   package Ypkg is
      Y : Real1;
   end Ypkg;

   package body Ypkg is
   begin
      Y := Real1'Last;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " & "REAL1'LAST");
      when others =>
         Failed ("OTHER EXCEPTION RAISED BY ASSIGNMENT OF " & "REAL1'LAST");
   end Ypkg;

   package Apkg is
      A : Real2;
   end Apkg;

   package body Apkg is
   begin
      A := Real2'First;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " & "REAL2'FIRST");
      when others =>
         Failed ("OTHER EXCEPTION RAISED BY ASSIGNMENT OF " & "REAL2'FIRST");
   end Apkg;

   package Bpkg is
      B : Real2;
   end Bpkg;

   package body Bpkg is
   begin
      B := Real2'Last;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " & "REAL2'LAST");
      when others =>
         Failed ("OTHER EXCEPTION RAISED BY ASSIGNMENT OF " & "REAL2'LAST");
   end Bpkg;

begin

   declare
      use Xpkg;
      use Ypkg;
   begin
      if X > Y then
         Failed ("REAL1'FIRST IS GREATER THAN REAL1'LAST");
      end if;
   end;

   declare
      use Apkg;
      use Bpkg;
   begin
      if A > B then
         Failed ("REAL2'FIRST IS GREATER THEN REAL2'LAST");
      end if;
   end;

   Result;

end C35703a;

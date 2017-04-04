-- C92005B.ADA

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
-- CHECK THAT FOR A TASK OBJECT CREATED BY AN ALLOCATOR THE OBJECT VALUE IS SET
-- DURING EXECUTION OF THE ALLOCATOR.

-- WEI  3/ 4/82
-- JBG  5/25/85
-- RLB  1/ 7/05

with Report; use Report;
with System;
procedure C92005b is
   type Big_Int is range 0 .. System.Max_Int;
begin
   Test ("C92005B", "TASK VALUE SET BY EXECUTION OF ALLOCATOR");

   Block : declare
      task type Tt1;

      type Att1 is access Tt1;

      task body Tt1 is
      begin
         null;
      end Tt1;

      package Pack is
      end Pack;

      package body Pack is
         Pointer_Tt1 : Att1    := new Tt1;
         I           : Big_Int := Pointer_Tt1.all'Storage_Size;
      begin
         if not Equal (Integer (I mod 1_024), Integer (I mod 1_024)) then
            Failed ("UNEXPECTED PROBLEM");
         end if;
      end Pack;
   begin
      null;
   exception
      when Program_Error | Constraint_Error =>
         Failed
           ("TASK OBJECT VALUE NOT SET DURING " & "EXECUTION OF ALLOCATOR");
   end Block;

   Result;

end C92005b;

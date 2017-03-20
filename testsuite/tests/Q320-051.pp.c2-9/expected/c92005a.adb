-- C92005A.ADA

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
-- CHECK THAT FOR A NON-SINGLE TASK THE OBJECT VALUE IS SET DURING
-- ELABORATION OF THE CORRESPONDING OBJECT DECLARATION.

-- WEI  3/ 4/82
-- JBG 5/25/85
-- PWB  2/3/86    CORRECTED TEST ERROR; ADDED 'USE' CLAUSE TO MAKE "/="
--                FOR BIG_INT VISIBLE.

with Report, System;
use Report;
procedure C92005a is
begin

   Test ("C92005A", "TASK OBJECT VALUE DURING ELABORATION");

   declare
      task type Tt1;

      Obj_Tt1 : Tt1;

      package Pack is
         type Big_Int is range 0 .. System.Max_Int;
         I : Big_Int;
      end Pack;

      package body Pack is
      begin
         I := Obj_Tt1'Storage_Size;  -- O.K.
      exception
         when others =>
            Failed ("TASK OBJECT RAISED EXCEPTION");
      end Pack;

      use Pack;

      task body Tt1 is
      begin
         null;
      end Tt1;

   begin
      if Pack.I /= Obj_Tt1'Storage_Size then
         Comment ("STORAGE SIZE CHANGED AFTER TASK ACTIVATED");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY STORAGE_SIZE");
   end;

   Result;
end C92005a;

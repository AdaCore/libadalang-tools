-- CD5014Y.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN IN THE VISIBLE PART
--     OF A GENERIC PACKAGE SPECIFICATION FOR A VARIABLE OF A FORMAL
--     PRIVATE TYPE, WHERE THE VARIABLE IS DECLARED IN THE VISIBLE PART
--     OF THE SPECIFICATION.

-- HISTORY:
--     BCB 10/08/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System;  use System;
with Spprt13; use Spprt13;
with Report;  use Report;

procedure Cd5014y is

begin

   Test
     ("CD5014Y",
      " AN ADDRESS CLAUSE CAN BE GIVEN " &
      "IN THE VISIBLE PART OF A GENERIC PACKAGE " &
      "SPECIFICATION FOR A VARIABLE OF A FORMAL " &
      "PRIVATE TYPE, WHERE THE VARIABLE IS DECLARED " &
      "IN THE VISIBLE PART OF THE SPECIFICATION");

   declare

      generic
         type Form_Private_Type is private;
      package Pkg is
         Form_Private_Obj1 : Form_Private_Type;
         for Form_Private_Obj1 use at Variable_Address;
      end Pkg;

      package body Pkg is
      begin
         if Form_Private_Obj1'Address /= Variable_Address then
            Failed ("INCORRECT ADDRESS FOR FORMAL PRIVATE " & "VARIABLE");
         end if;
      end Pkg;

      package Pack is new Pkg (Form_Private_Type => Integer);

   begin
      null;
   end;

   Result;
end Cd5014y;

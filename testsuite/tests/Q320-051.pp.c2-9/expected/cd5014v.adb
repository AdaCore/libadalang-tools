-- CD5014V.ADA

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
--     FIXED TYPE, WHERE THE VARIABLE IS DECLARED IN THE VISIBLE PART
--     OF THE SPECIFICATION.

-- HISTORY:
--     BCB 10/08/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System;  use System;
with Spprt13; use Spprt13;
with Report;  use Report;

procedure Cd5014v is

begin

   Test
     ("CD5014V",
      " AN ADDRESS CLAUSE CAN BE GIVEN " &
      "IN THE VISIBLE PART OF A GENERIC PACKAGE " &
      "SPECIFICATION FOR A VARIABLE OF A FORMAL " &
      "FIXED TYPE, WHERE THE VARIABLE IS DECLARED " &
      "IN THE VISIBLE PART OF THE SPECIFICATION");

   declare
      type Fix is delta 0.5 range -30.00 .. 30.00;

      generic
         type Form_Fixed_Type is delta <>;
      package Pkg is
         Form_Fixed_Obj1 : Form_Fixed_Type := 5.0;
         for Form_Fixed_Obj1 use at Variable_Address;
      end Pkg;

      package body Pkg is
      begin
         if Equal (3, 3) then
            Form_Fixed_Obj1 := 20.0;
         end if;

         if Form_Fixed_Obj1 /= 20.0 then
            Failed ("INCORRECT VALUE FOR FORMAL FIXED VARIABLE");
         end if;

         if Form_Fixed_Obj1'Address /= Variable_Address then
            Failed ("INCORRECT ADDRESS FOR FORMAL FIXED " & "VARIABLE");
         end if;
      end Pkg;

      package Pack is new Pkg (Form_Fixed_Type => Fix);

   begin
      null;
   end;

   Result;
end Cd5014v;

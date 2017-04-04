-- CD5014X.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN IN THE PRIVATE PART
--     OF A GENERIC PACKAGE SPECIFICATION FOR A VARIABLE OF A FORMAL
--     ARRAY TYPE, WHERE THE VARIABLE IS DECLARED IN THE VISIBLE PART
--     OF THE SPECIFICATION.

-- HISTORY:
--     BCB 10/08/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System;  use System;
with Spprt13; use Spprt13;
with Report;  use Report;
with Text_Io; use Text_Io;

procedure Cd5014x is

begin

   Test
     ("CD5014X",
      " AN ADDRESS CLAUSE CAN BE GIVEN " &
      "IN THE PRIVATE PART OF A GENERIC PACKAGE " &
      "SPECIFICATION FOR A VARIABLE OF A FORMAL " &
      "ARRAY TYPE, WHERE THE VARIABLE IS DECLARED " &
      "IN THE VISIBLE PART OF THE SPECIFICATION");

   declare

      type Color is (Red, Blue, Green);
      type Color_Table is array (Color) of Integer;

      generic
         type Index is (<>);
         type Form_Array_Type is array (Index) of Integer;
      package Pkg is
         Form_Array_Obj1 : Form_Array_Type := (1, 2, 3);
      private
         for Form_Array_Obj1 use at Variable_Address;
      end Pkg;

      package body Pkg is
      begin

         if Equal (3, 3) then
            Form_Array_Obj1 := (10, 20, 30);
         end if;

         if Form_Array_Obj1 /= (10, 20, 30) then
            Failed ("INCORRECT VALUE FOR FORMAL ARRAY VARIABLE");
         end if;

         if Form_Array_Obj1'Address /= Variable_Address then
            Failed ("INCORRECT ADDRESS FOR FORMAL ARRAY " & "VARIABLE");
         end if;
      end Pkg;

      package Pack is new Pkg (Index => Color, Form_Array_Type => Color_Table);

   begin
      null;
   end;

   Result;
end Cd5014x;

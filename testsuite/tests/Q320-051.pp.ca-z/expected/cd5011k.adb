-- CD5011K.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN FOR A VARIABLE OF A
--     RECORD TYPE IN THE DECLARATIVE PART OF A BLOCK STATEMENT.

-- HISTORY:
--     JET 09/15/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System; use System;
with Report; use Report;
with Spprt13;

procedure Cd5011k is

begin

   Test
     ("CD5011K",
      "AN ADDRESS CLAUSE CAN BE " &
      "GIVEN FOR A VARIABLE OF A RECORD " &
      "TYPE IN THE DECLARATIVE PART OF A " &
      "BLOCK STATEMENT");

   declare

      type Rec_Type is record
         I : Integer := 12;
         B : Boolean := True;
      end record;

      Rec : Rec_Type;
      for Rec use at Spprt13.Variable_Address;

   begin
      if Equal (3, 3) then
         Rec.I := 17;
         Rec.B := False;
      end if;

      if Rec.I /= 17 or Rec.B then
         Failed ("WRONG VALUE FOR VARIABLE IN BLOCK");
      end if;

      if Rec'Address /= Spprt13.Variable_Address then
         Failed ("WRONG ADDRESS FOR VARIABLE IN BLOCK");
      end if;

   end;

   Result;

end Cd5011k;

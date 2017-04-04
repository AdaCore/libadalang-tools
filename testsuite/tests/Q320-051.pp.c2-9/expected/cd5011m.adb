-- CD5011M.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN FOR A VARIABLE OF
--     AN ACCESS TYPE IN THE DECLARATIVE PART OF A SUBPROGRAM.

-- HISTORY:
--     JET 09/15/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System; use System;
with Report; use Report;
with Spprt13;

procedure Cd5011m is

   type Acc_Type is access String;

   procedure Cd5011m_Proc is

      Acc : Acc_Type := new String'("THE QUICK BROWN FOX");
      for Acc use at Spprt13.Variable_Address;

   begin
      if Equal (3, 3) then
         Acc := new String'("THE LAZY DOG");
      end if;

      if Acc.all /= Ident_Str ("THE LAZY DOG") then
         Failed ("INCORRECT VALUE FOR VARIABLE IN PROCEDURE");
      end if;

      if Acc'Address /= Spprt13.Variable_Address then
         Failed ("INCORRECT ADDRESS FOR VARIABLE IN PROCEDURE");
      end if;

   end Cd5011m_Proc;

begin
   Test
     ("CD5011M",
      "AN ADDRESS CLAUSE CAN BE " &
      "GIVEN FOR A VARIABLE OF AN ACCESS " &
      "TYPE IN THE DECLARATIVE PART OF A " &
      "SUBPROGRAM");

   Cd5011m_Proc;

   Result;

end Cd5011m;

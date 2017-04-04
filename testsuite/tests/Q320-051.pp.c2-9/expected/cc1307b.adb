-- CC1307B.ADA

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
--     CHECK THAT AN ENUMERATION LITERAL (BOTH AN IDENTIFIER AND A
--     CHARACTER LITERAL) MAY BE USED AS A DEFAULT SUBPROGRAM NAME
--     AND AS A DEFAULT INITIAL VALUE FOR AN OBJECT PARAMETER.

-- HISTORY:
--     BCB 08/09/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure Cc1307b is

   type Enum is (R, 'S', R1);

begin
   Test
     ("CC1307B",
      "CHECK THAT AN ENUMERATION LITERAL (BOTH AN " &
      "IDENTIFIER AND A CHARACTER LITERAL) MAY BE " &
      "USED AS A DEFAULT SUBPROGRAM NAME AND AS A " &
      "DEFAULT INITIAL VALUE FOR AN OBJECT PARAMETER");

   declare
      generic
         with function J return Enum is R;
         with function K return Enum is 'S';
         Obj1 : Enum := R;
         Obj2 : Enum := 'S';
      package P is
      end P;

      package body P is
         Var1, Var2 : Enum := R1;
      begin
         Var1 := J;

         if Var1 /= R then
            Failed
              ("WRONG VALUE FOR DEFAULT SUBPROGRAM " & "NAME - IDENTIFIER");
         end if;

         Var2 := K;

         if Var2 /= 'S' then
            Failed
              ("WRONG VALUE FOR DEFAULT SUBPROGRAM " &
               "NAME - CHARACTER LITERAL");
         end if;

         if Obj1 /= R then
            Failed ("WRONG VALUE FOR OBJECT PARAMETER - " & "IDENTIFIER");
         end if;

         if Obj2 /= 'S' then
            Failed
              ("WRONG VALUE FOR OBJECT PARAMETER - " & "CHARACTER LITERAL");
         end if;
      end P;

      package New_P is new P;
   begin
      null;
   end;

   Result;
end Cc1307b;

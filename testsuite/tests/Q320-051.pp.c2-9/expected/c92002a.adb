-- C92002A.ADA

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
-- CHECK THAT ASSIGNMENT TO A COMPONENT (FOR WHICH ASSIGNMENT IS
--   AVAILABLE) OF A RECORD CONTAINING A TASK IS AVAILABLE.

-- JRK 9/17/81
-- JWC 6/28/85 RENAMED TO -AB

with Report; use Report;
procedure C92002a is

begin
   Test
     ("C92002A",
      "CHECK THAT CAN ASSIGN TO ASSIGNABLE " &
      "COMPONENTS OF RECORDS WITH TASK " & "COMPONENTS");

   declare

      task type Tt is
         entry E;
      end Tt;

      type Rt is record
         I : Integer := 0;
         T : Tt;
         J : Integer := 0;
      end record;

      R : Rt;

      task body Tt is
      begin
         null;
      end Tt;

   begin

      R.I := Ident_Int (7);
      R.J := Ident_Int (9);

      if R.I /= 7 and R.J /= 9 then
         Failed
           ("WRONG VALUE(S) WHEN ASSIGNING TO " &
            "INTEGER COMPONENTS OF RECORDS WITH " & "TASK COMPONENTS");
      end if;

   end;

   Result;
end C92002a;

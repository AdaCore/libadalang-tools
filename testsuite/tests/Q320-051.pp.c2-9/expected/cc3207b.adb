-- CC3207B.ADA

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
--     CHECK THAT INSTANTIATION IS LEGAL IF A FORMAL
--     PARAMETER HAVING A LIMITED PRIVATE TYPE WITHOUT
--     A DISCRIMINANT IS USED TO DECLARE AN ACCESS
--     TYPE IN A BLOCK THAT CONTAINS A SELECTIVE WAIT
--     WITH A TERMINATE ALTERNATIVE, AND ACTUAL
--     PARAMETER'S BASE IS A TASK TYPE OR TYPE WITH A
--     SUBCOMPONENT OF A TASK TYPE.

-- HISTORY:
--     LDC  06/24/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure Cc3207b is
begin
   Test
     ("CC3207B",
      "CHECK THAT INSTANTIATION IS LEGAL IF A " &
      "FORMAL PARAMETER HAVING A LIMITED PRIVATE " &
      "TYPE WITHOUT A DISCRIMINANT IS USED TO " &
      "DECLARE AN ACCESS TYPE IN A BLOCK THAT " &
      "CONTAINS A SELECTIVE WAIT WITH A TERMINATE " &
      "ALTERNATIVE, AND ACTUAL PARAMETER'S BASE " &
      "A TASK TYPE OR TYPE WITH A SUBCOMPONENT OF " &
      "A TASK TYPE. ");

   declare
      task type Tt is
         entry E;
      end Tt;

      type Tt_Arr is array (1 .. 2) of Tt;

      type Tt_Rec is record
         Comp : Tt_Arr;
      end record;

      generic
         type T is limited private;
      package Gen is
         task Tsk is
            entry Ent (A : out Integer);
         end Tsk;
      end Gen;

      Int : Integer;

      task body Tt is
      begin
         select
            accept E;
         or
            terminate;
         end select;
      end Tt;

      package body Gen is
         task body Tsk is
         begin
            declare
               type Acc_T is access T;
               Ta : Acc_T := new T;
            begin
               select
                  accept Ent (A : out Integer) do
                     A := Ident_Int (7);
                  end Ent;
               or
                  terminate;
               end select;
            end;
         end Tsk;
      end Gen;

      package Gen_Tsk is new Gen (Tt);
      package Gen_Tsk_Sub is new Gen (Tt_Rec);

   begin
      Gen_Tsk.Tsk.Ent (Int);

      if Int /= Ident_Int (7) then
         Failed ("THE WRONG VALUE WAS RETURNED BY THE TASK");
      end if;

      Int := 0;
      Gen_Tsk_Sub.Tsk.Ent (Int);

      if Int /= Ident_Int (7) then
         Failed
           ("THE WRONG VALUE WAS RETURNED BY THE TASK, " &
            "WITH ACTUAL PARAMETER'S BASE IS A SUB" &
            "COMPONENT OF A TASK TYPE");
      end if;
      Result;
   end;
end Cc3207b;

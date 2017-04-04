-- C64201B.ADA

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
-- CHECK THAT INITALIZATION OF IN PARAMETERS OF A TASK
-- TYPE IS PERMITTED.
-- (SEE ALSO 7.4.4/T2 FOR TESTS OF LIMITED PRIVATE TYPES.)

-- CVP 5/14/81
-- ABW 7/1/82
-- BHS 7/9/84

with Report;
procedure C64201b is

   use Report;

begin

   Test
     ("C64201B",
      "CHECK THAT INITIALIZATION OF IN PARAMETERS " &
      "OF A TASK TYPE IS PERMITTED");

   declare

      Global : Integer := 10;

      task type T_Type is
         entry E (X : in out Integer);
      end T_Type;

      Tsk1, Tsk2 : T_Type;

      task body T_Type is
      begin
         accept E (X : in out Integer) do
            X := X - 1;
         end E;
         accept E (X : in out Integer) do
            X := X + 1;
         end E;
      end T_Type;

      procedure Proc1 (T : T_Type := Tsk1) is
      begin
         T.E (X => Global);
      end Proc1;

      procedure Proc2 (T : T_Type := Tsk1) is
      begin
         T.E (X => Global);
         if (Global /= Ident_Int (8)) then
            Failed ("TASK NOT PASSED IN PROC1, " & "DEFAULT TSK1 EMPLOYED");
         end if;
      end Proc2;

      procedure Term (T : T_Type; Num : Character) is
      begin
         if not T'Terminated then
            abort T;
            Comment ("ABORTING TASK " & Num);
         end if;
      end Term;

   begin

      Proc1 (Tsk2);
      if Global /= 9 then
         Failed ("INCORRECT GLOBAL VALUE AFTER PROC1");
      else
         Proc2;
      end if;

      Term (Tsk1, '1');
      Term (Tsk2, '2');
   end;

   Result;

end C64201b;

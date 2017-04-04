-- C95078A.ADA

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
--     CHECK THAT AN EXCEPTION RAISED DURING THE EXECUTION OF AN ACCEPT
--     STATEMENT CAN BE HANDLED WITHIN THE ACCEPT BODY.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     DHH 03/21/88 CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report; use Report;
procedure C95078a is

begin

   Test
     ("C95078A",
      "CHECK THAT AN EXCEPTION RAISED DURING THE " &
      "EXECUTION OF AN ACCEPT STATEMENT CAN BE " &
      "HANDLED WITHIN THE ACCEPT BODY");

   declare
      O, Pt, Qt, R, S, Tp, B, C, D : Integer := 0;
      task type Prog_Err is
         entry Start (M, N, A : in out Integer);
         entry Stop;
      end Prog_Err;

      task T is
         entry Start (M, N, A : in out Integer);
         entry Stop;
      end T;

      type Rec is record
         B : Prog_Err;
      end record;

      type Acc is access Prog_Err;

      subtype X is Integer range 1 .. 10;

      package P is
         Obj : Rec;
      end P;

      task body Prog_Err is
         Fault : X;
      begin
         accept Start (M, N, A : in out Integer) do
            begin
               M     := Ident_Int (1);
               Fault := Ident_Int (11);
               Fault := Ident_Int (Fault);
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed
                    ("UNEXPECTED ERROR RAISED - " & "CONSTRAINT - TASK TYPE");
            end; -- EXCEPTION
            begin
               N     := Ident_Int (1);
               Fault := Ident_Int (5);
               Fault := Fault / Ident_Int (0);
               Fault := Ident_Int (Fault);
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed
                    ("UNEXPECTED ERROR RAISED - " & "CONSTRAINT - TASK TYPE");
            end; -- EXCEPTION
            A := Ident_Int (1);
         end Start;

         accept Stop;
      end Prog_Err;

      task body T is
         Fault : X;
      begin
         accept Start (M, N, A : in out Integer) do
            begin
               M     := Ident_Int (1);
               Fault := Ident_Int (11);
               Fault := Ident_Int (Fault);
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED ERROR RAISED - " & "CONSTRAINT - TASK");
            end; -- EXCEPTION
            begin
               N     := Ident_Int (1);
               Fault := Ident_Int (5);
               Fault := Fault / Ident_Int (0);
               Fault := Ident_Int (Fault);
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED ERROR RAISED - " & "CONSTRAINT - TASK");
            end; -- EXCEPTION
            A := Ident_Int (1);
         end Start;

         accept Stop;
      end T;

      package body P is
      begin
         Obj.B.Start (O, Pt, B);
         Obj.B.Stop;

         if O /= Ident_Int (1) or Pt /= Ident_Int (1) then
            Failed
              ("EXCEPTION HANDLER NEVER ENTERED " &
               "PROPERLY - TASK TYPE OBJECT");
         end if;

         if B /= Ident_Int (1) then
            Failed ("TASK NOT EXITED PROPERLY - TASK TYPE " & "OBJECT");
         end if;
      end P;

      package Q is
         Obj : Acc;
      end Q;

      package body Q is
      begin
         Obj := new Prog_Err;
         Obj.Start (Qt, R, C);
         Obj.Stop;

         if Qt /= Ident_Int (1) or R /= Ident_Int (1) then
            Failed
              ("EXCEPTION HANDLER NEVER ENTERED " &
               "PROPERLY - ACCESS TASK TYPE");
         end if;

         if C /= Ident_Int (1) then
            Failed ("TASK NOT EXITED PROPERLY - ACCESS TASK " & "TYPE");
         end if;
      end Q;

   begin
      T.Start (S, Tp, D);
      T.Stop;

      if S /= Ident_Int (1) or Tp /= Ident_Int (1) then
         Failed ("EXCEPTION HANDLER NEVER ENTERED PROPERLY " & "- TASK");
      end if;

      if D /= Ident_Int (1) then
         Failed ("TASK NOT EXITED PROPERLY - TASK");
      end if;
   end; -- DECLARE

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT HANDLED INSIDE ACCEPT BODY");
      Result;
end C95078a;

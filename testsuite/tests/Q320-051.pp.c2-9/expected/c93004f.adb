-- C93004F.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED DURING THE ACTIVATION OF A TASK,
-- OTHER TASKS ARE UNAFFECTED.

-- THE ENCLOSING BLOCK RECEIVES TASKING_ERROR.

-- THIS TESTS CHECKS THE CASE IN WHICH THE TASKS ARE CREATED BY THE ALLOCATION
-- OF A RECORD OF TASKS OR AN ARRAY OF TASKS.

-- R. WILLIAMS 8/7/86

with Report; use Report;

procedure C93004f is

begin
   Test
     ("C93004F",
      "CHECK THAT WHEN AN EXCEPTION IS RAISED " &
      "DURING THE ACTIVATION OF A TASK, OTHER " &
      "TASKS ARE UNAFFECTED. IN THIS TEST, THE " &
      "TASKS ARE CREATED BY THE ALLOCATION OF A " &
      "RECORD OR AN ARRAY OF TASKS");

   declare

      task type T is
         entry E;
      end T;

      task type Tt;

      task type Tx is
         entry E;
      end Tx;

      type Rec is record
         Tr : T;
      end record;

      type Arr is array (Ident_Int (1) .. Ident_Int (1)) of T;

      type Recx is record
         Ttx1 : Tx;
         Ttt  : Tt;
         Ttx2 : Tx;
      end record;

      type Accr is access Rec;
      Ar : Accr;

      type Acca is access Arr;
      Aa : Acca;

      type Accx is access Recx;
      Ax : Accx;

      task body T is
      begin
         accept E;
      end T;

      task body Tt is
      begin
         Ar.Tr.E;
      exception
         when others =>
            Failed ("TASK AR.TR NOT ACTIVE");
      end Tt;

      task body Tx is
         I : Positive := Ident_Int (0); -- RAISE
      -- CONSTRAINT_ERROR.
      begin
         if I /= Ident_Int (2) or I = Ident_Int (1) + 1 then
            Failed ("TX ACTIVATED OK");
         end if;
      end Tx;

   begin
      Ar := new Rec;
      Aa := new Arr;
      Ax := new Recx;

      Failed ("TASKING_ERROR NOT RAISED IN MAIN");

      Aa.all (1).E;        -- CLEAN UP.

   exception
      when Tasking_Error =>

         begin
            Aa.all (1).E;
         exception
            when Tasking_Error =>
               Failed ("AA.ALL (1) NOT ACTIVATED");
         end;

      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED IN MAIN");
      when others =>
         Failed ("ABNORMAL EXCEPTION IN MAIN");
   end;

   Result;

end C93004f;

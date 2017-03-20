-- C74208B.ADA

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
--     CHECK THAT 'CONSTRAINED FOR OBJECTS OF A PRIVATE TYPE WITH
--     VISIBLE DISCRIMINANTS IS AVAILABLE OUTSIDE THE PACKAGE DECLARING
--     THE TYPE AND IS AVAILABLE BEFORE AND AFTER THE FULL DECLARATION.

-- HISTORY:
--     BCB 07/14/88  CREATED ORIGINAL TEST.
--     GJD 11/15/95  MOVED REC2_VAR OUT OF P DUE TO ADA 95 FREEZING RULES.

with Report; use Report;

procedure C74208b is

   package P is
      type Rec (D : Integer := 0) is private;
      R1 : constant Rec;
      type Rec2 is record
         Comp : Boolean := R1'Constrained;
      end record;
   private
      type Rec (D : Integer := 0) is record
         null;
      end record;
      R1    : constant Rec := (D => 5);
      R2    : Rec          := (D => 0);
      R2a   : Rec (3);
      R2con : constant Rec := (D => 3);
      C     : Boolean      := R2'Constrained;
      D     : Boolean      := R2a'Constrained;
      E     : Boolean      := R2con'Constrained;
   end P;

   Rec2_Var : P.Rec2;

   R3  : P.Rec (0);
   R3a : P.Rec;

   A : Boolean := R3'Constrained;
   B : Boolean := R3a'Constrained;

   package body P is
   begin
      Test
        ("C74208B",
         "CHECK THAT 'CONSTRAINED FOR OBJECTS OF A " &
         "PRIVATE TYPE WITH VISIBLE DISCRIMINANTS " &
         "IS AVAILABLE OUTSIDE THE PACKAGE " &
         "DECLARING THE TYPE AND IS AVAILABLE " &
         "BEFORE AND AFTER THE FULL DECLARATION");

      if not Rec2_Var.Comp then
         Failed
           ("IMPROPER VALUE FOR 'CONSTRAINED BEFORE THE " &
            "FULL DECLARATION OF THE PRIVATE TYPE");
      end if;

      if C then
         Failed
           ("IMPROPER VALUE FOR 'CONSTRAINED AFTER THE " &
            "FULL DECLARATION OF THE PRIVATE TYPE - 1");
      end if;

      if not D then
         Failed
           ("IMPROPER VALUE FOR 'CONSTRAINED AFTER THE " &
            "FULL DECLARATION OF THE PRIVATE TYPE - 2");
      end if;

      if not E then
         Failed
           ("IMPROPER VALUE FOR 'CONSTRAINED AFTER THE " &
            "FULL DECLARATION OF THE PRIVATE TYPE - 3");
      end if;
   end P;

begin
   if not A then
      Failed
        ("IMPROPER VALUE FOR 'CONSTRAINED OUTSIDE THE " &
         "PACKAGE DECLARING THE PRIVATE TYPE - 1");
   end if;

   if B then
      Failed
        ("IMPROPER VALUE FOR 'CONSTRAINED OUTSIDE THE " &
         "PACKAGE DECLARING THE PRIVATE TYPE - 2");
   end if;

   Result;
end C74208b;

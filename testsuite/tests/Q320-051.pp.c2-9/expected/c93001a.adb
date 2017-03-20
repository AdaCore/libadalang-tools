-- C93001A.ADA

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
-- CHECK THAT DECLARED TASK OBJECTS ARE NOT ACTIVATED BEFORE
--   THE END OF THE DECLARATIVE PART.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (B)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (C)  A RECORD OF TASK OBJECT, IN A PACKAGE SPECIFICATION.
--   (D)  A RECORD OF ARRAY OF TASK OBJECT, IN A PACKAGE BODY.
--   (E)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- THIS TEST ASSUMES THAT ACTIVATION IS A SEQUENTIAL STEP
--   IN THE FLOW OF CONTROL OF THE PARENT (AS IS REQUIRED BY THE
--   ADA RM).  IF AN IMPLEMENTATION (ILLEGALLY) ACTIVATES A
--   TASK IN PARALLEL WITH ITS PARENT, THIS TEST
--   IS NOT GUARANTEED TO DETECT THE VIOLATION, DUE TO A
--   RACE CONDITION.

-- JRK 9/23/81
-- SPS 11/1/82
-- SPS 11/21/82
-- R.WILLIAMS 10/8/86  ADDED CHECKS ON INITIALIZATIONS OF NON-TASK
--                     COMPONENTS OF RECORD TYPES.
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C93001a is

   Global : Integer;

   function Side_Effect (I : Integer) return Integer is
   begin
      Global := Ident_Int (I);
      return 0;
   end Side_Effect;

   task type Tt is
      entry E;
   end Tt;

   task body Tt is
      I : Integer := Side_Effect (1);
   begin
      null;
   end Tt;

begin
   Test
     ("C93001A",
      "CHECK THAT DECLARED TASK OBJECTS ARE NOT " &
      "ACTIVATED BEFORE THE END OF THE DECLARATIVE " &

      "PART");

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (A)

      T : Tt;
      I : Integer := Global;

   begin -- (A)

      if I /= 0 then
         Failed
           ("A SIMPLE TASK OBJECT IN A BLOCK WAS " &
            "ACTIVATED TOO SOON - (A)");
      end if;

   end; -- (A)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (B)

      J : Integer;

      function F return Integer is
         A : array (1 .. 1) of Tt;
         I : Integer := Global;
      begin
         if I /= 0 then
            Failed
              ("AN ARRAY OF TASK OBJECT IN A FUNCTION " &
               "WAS ACTIVATED TOO SOON - (B)");
         end if;
         return 0;
      end F;

   begin -- (B)

      J := F;

   end; -- (B)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C)

      package P is

         type Rec is record
            T  : Tt;
            N1 : Integer := Global;
         end record;

         type Rt is record
            M : Integer := Global;
            T : Tt;
            N : Rec;
         end record;
         R : Rt;
         I : Integer := Global;
      end P;

      package Q is
         J : Integer;
      private
         type Rt is record
            N : P.Rec;
            T : Tt;
            M : Integer := Global;
         end record;
         R : Rt;
      end Q;

      K : Integer := Global;

      package body Q is
      begin
         if R.M /= 0 or R.N.N1 /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD R NOT " &
               "INITIALIZED BEFORE TASKS ACTIVATED " &
               "- (C.1)");
         end if;
      end Q;

   begin -- (C)

      if P.R.M /= 0 or P.R.N.N1 /= 0 then
         Failed
           ("NON-TASK COMPONENTS OF RECORDS NOT " &
            "INITIALIZED BEFORE TASKS ACTIVATED " &
            "- (C.2)");
      end if;

      if P.I /= 0 or K /= 0 then
         Failed
           ("A RECORD OF TASK OBJECT IN A PACKAGE " &
            "SPECIFICATION WAS ACTIVATED TOO SOON - (C)");
      end if;

   end; -- (C)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (D)

      package P is

         type Grade is (Good, Fair, Poor);

         type Rec (G : Grade) is record
            null;
         end record;

         type Accr is access Rec;
         type Acci is access Integer;

         type Arr is array (1 .. 1) of Tt;
         type Rat is record
            M : Accr := new Rec (Grade'Val (Global));
            A : Arr;
            N : Acci := new Integer'(Global);
         end record;
         Ra1 : Rat;
      private
         Ra2 : Rat;
      end P;

      package body P is
         Ra3 : Rat;
         I   : Integer := Global;
      begin
         if Ra1.M.G /= Good or Ra1.N.all /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD RA1 NOT " &
               "INITIALIZED BEFORE TASKS ACTIVATED " &
               "- (D)");
         end if;

         if Ra2.M.G /= Good or Ra2.N.all /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD RA2 NOT " &
               "INITIALIZED BEFORE TASKS ACTIVATED " &
               "- (D)");
         end if;

         if Ra3.M.G /= Good or Ra3.N.all /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD RA3 NOT " &
               "INITIALIZED BEFORE TASKS ACTIVATED " &
               "- (D)");
         end if;

         if I /= 0 then
            Failed
              ("A RECORD OF ARRAY OF TASK OBJECT IN A " &
               "PACKAGE SPEC OR BODY WAS ACTIVATED " &
               "TOO SOON - (D)");
         end if;
      end P;

   begin -- (D)

      null;

   end; -- (D)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (E)

      type Rec is record
         B : Boolean   := Boolean'Val (Global);
         T : Tt;
         C : Character := Character'Val (Global);
      end record;

      task T is
         entry E;
      end T;

      task body T is
         type Rt is record
            M : Rec;
            T : Tt;
            N : Rec;
         end record;
         Ar : array (1 .. 1) of Rt;
         I  : Integer := Global;
      begin
         if Ar (1).M.B /= False or
           Ar (1).M.C /= Ascii.Nul or
           Ar (1).N.B /= False or
           Ar (1).N.C /= Ascii.Nul
         then
            Failed
              ("NON-TASK COMPONENTS OF RECORD RT NOT " &
               "INITIALIZED BEFORE TASKS ACTIVATED " &
               "- (E)");
         end if;

         if I /= 0 then
            Failed
              ("AN ARRAY OF RECORD OF TASK OBJECT IN A " &
               "TASK BODY WAS ACTIVATED TOO SOON - (E)");
         end if;
      end T;

   begin -- (E)

      null;

   end; -- (E)

   --------------------------------------------------

   Result;
end C93001a;

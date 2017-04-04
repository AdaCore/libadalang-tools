-- C93003A.ADA

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
-- CHECK THAT ACTIVATION OF TASKS CREATED BY ALLOCATORS PRESENT IN A
--   DECLARATIVE PART TAKES PLACE DURING ELABORATION OF THE
--   CORRESPONDING DECLARATION.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--   (B)  AN ARRAY OF TASK ALLOCATOR, IN A FUNCTION.
--   (C)  A RECORD OF TASK ALLOCATOR, IN A PACKAGE SPECIFICATION.
--   (D)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A PACKAGE BODY.
--   (E)  AN ARRAY OF RECORD OF TASK ALLOCATOR, IN A TASK BODY.

-- JRK 9/28/81
-- SPS 11/11/82
-- SPS 11/21/82
-- RJW 8/4/86 ADDED CHECKS ON INITIALIZATIONS OF NON-TASK COMPONENTS
--               OF RECORD TYPES.
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C93003a is

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
     ("C93003A",
      "CHECK THAT ACTIVATION OF TASKS CREATED BY " &
      "ALLOCATORS PRESENT IN A DECLARATIVE PART " &
      "TAKES PLACE DURING ELABORATION OF THE " &
      "CORRESPONDING DECLARATION");

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (A)

      type A is access Tt;
      T1 : A       := new Tt;
      I1 : Integer := Global;
      J  : Integer := Side_Effect (0);
      T2 : A       := new Tt;
      I2 : Integer := Global;

   begin -- (A)

      if I1 /= 1 or I2 /= 1 then
         Failed
           ("A SIMPLE TASK ALLOCATOR IN A BLOCK WAS " &
            "ACTIVATED TOO LATE - (A)");
      end if;

   end; -- (A)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (B)

      J : Integer;

      function F return Integer is

         type A_T is array (1 .. 1) of Tt;
         type A is access A_T;
         A1 : A       := new A_T;
         I1 : Integer := Global;
         J  : Integer := Side_Effect (0);
         A2 : A       := new A_T;
         I2 : Integer := Global;

      begin
         if I1 /= 1 or I2 /= 1 then
            Failed
              ("AN ARRAY OF TASK ALLOCATOR IN A " &
               "FUNCTION WAS ACTIVATED TOO LATE - (B)");
         end if;
         return 0;
      end F;

   begin -- (B)

      J := F;

   end; -- (B)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C1)

      package P is

         type Intrec is record
            N1 : Integer := Global;
         end record;

         type Rt is record
            M : Integer := Global;
            T : Tt;
            N : Intrec;
         end record;

         type A is access Rt;

         R1 : A       := new Rt;
         I1 : Integer := Global;
         J  : Integer := Side_Effect (0);
         R2 : A       := new Rt;
         I2 : Integer := Global;

      end P;

   begin -- (C1)

      if P.R1.M /= 0 or P.R1.N.N1 /= 0 then
         Failed
           ("NON-TASK COMPONENTS OF RECORD R1 NOT " &
            "INITIALIZED BEFORE TASK ACTIVATED - (C1)");
      end if;

      if P.R2.M /= 0 or P.R2.N.N1 /= 0 then
         Failed
           ("NON-TASK COMPONENTS OF RECORD R2 NOT " &
            "INITIALIZED BEFORE TASK ACTIVATED - (C1)");
      end if;

      if P.I1 /= 1 or P.I2 /= 1 then
         Failed
           ("A RECORD OF TASK ALLOCATOR IN A PACKAGE " &
            "SPECIFICATION WAS ACTIVATED TOO LATE - (C1)");
      end if;

   end; -- (C1)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C2)

      package Q is
         J1 : Integer;
      private

         type Grade is (Good, Fair, Poor);

         type Rec (G : Grade) is record
            null;
         end record;

         type Accr is access Rec;

         type Acci is access Integer;

         type Rt is record
            M : Accr := new Rec (Grade'Val (Global));
            T : Tt;
            N : Acci := new Integer'(Global);
         end record;

         type A is access Rt;

         R1 : A       := new Rt;
         I1 : Integer := Global;
         J2 : Integer := Side_Effect (0);
         R2 : A       := new Rt;
         I2 : Integer := Global;

      end Q;

      package body Q is
      begin
         if R1.M.G /= Good or R1.N.all /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD R1 NOT " &
               "INITIALIZED BEFORE TASK ACTIVATED " &
               "- (C2)");
         end if;

         if R2.M.G /= Good or R2.N.all /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD R2 NOT " &
               "INITIALIZED BEFORE TASK ACTIVATED " &
               "- (C2)");
         end if;

         if I1 /= 1 or I2 /= 1 then
            Failed
              ("A RECORD OF TASK ALLOCATOR IN A PACKAGE " &
               "SPECIFICATION WAS ACTIVATED TOO LATE " &
               "- (C2)");
         end if;
      end Q;

   begin -- (C2)

      null;

   end; -- (C2)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (D)

      package P is

         type Arr is array (1 .. 1) of Tt;
         type Intarr is array (1 .. 1) of Integer;

         type Rat is record
            M : Intarr := (1 => Global);
            A : Arr;
            N : Intarr := (1 => Global);
         end record;
      end P;

      package body P is

         type A is access Rat;

         Ra1 : A       := new Rat;
         I1  : Integer := Global;
         J   : Integer := Side_Effect (0);
         Ra2 : A       := new Rat;
         I2  : Integer := Global;

      begin
         if Ra1.M (1) /= 0 or Ra1.N (1) /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD RA1 NOT " &
               "INITIALIZED BEFORE TASK ACTIVATED " &
               "- (D)");
         end if;

         if Ra2.M (1) /= 0 or Ra2.N (1) /= 0 then
            Failed
              ("NON-TASK COMPONENTS OF RECORD RA2 NOT " &
               "INITIALIZED BEFORE TASK ACTIVATED " &
               "- (D)");
         end if;

         if I1 /= 1 or I2 /= 1 then
            Failed
              ("A RECORD OF ARRAY OF TASK ALLOCATOR IN " &
               "A PACKAGE BODY WAS ACTIVATED " &
               "TOO LATE - (D)");
         end if;
      end P;

   begin -- (D)

      null;

   end; -- (D)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (E)

      task T is
         entry E;
      end T;

      task body T is
         type Rt is record
            M : Boolean   := Boolean'Val (Global);
            T : Tt;
            N : Character := Character'Val (Global);
         end record;

         type Art is array (1 .. 1) of Rt;
         type A is access Art;

         Ar1 : A       := new Art;
         I1  : Integer := Global;
         J   : Integer := Side_Effect (0);
         Ar2 : A       := new Art;
         I2  : Integer := Global;

      begin
         if Ar1.all (1).M /= False or Ar1.all (1).N /= Ascii.Nul then
            Failed
              ("NON-TASK COMPONENTS OF RECORD AR1 NOT " &
               "INITIALIZED BEFORE TASK ACTIVATED " &
               "- (E)");
         end if;

         if Ar2.all (1).M /= False or Ar2.all (1).N /= Ascii.Nul then
            Failed
              ("NON-TASK COMPONENTS OF RECORD AR2 NOT " &
               "INITIALIZED BEFORE TASK ACTIVATED " &
               "- (E)");
         end if;

         if I1 /= 1 or I2 /= 1 then
            Failed
              ("AN ARRAY OF RECORD OF TASK ALLOCATOR IN " &
               "A TASK BODY WAS ACTIVATED TOO LATE - (E)");
         end if;
      end T;

   begin -- (E)

      null;

   end; -- (E)

   --------------------------------------------------

   Result;
end C93003a;

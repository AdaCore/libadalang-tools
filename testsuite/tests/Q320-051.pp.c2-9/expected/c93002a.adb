-- C93002A.ADA

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
-- CHECK THAT DECLARED TASK OBJECTS ARE ACTIVATED BEFORE EXECUTION
--   OF THE FIRST STATEMENT FOLLOWING THE DECLARATIVE PART.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (B)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (C)  A RECORD OF TASK OBJECT, IN A PACKAGE SPECIFICATION.
--   (D)  A RECORD OF ARRAY OF TASK OBJECT, IN A PACKAGE BODY.
--   (E)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- JRK 9/28/81
-- SPS 11/1/82
-- SPS 11/21/82
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C93002a is

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
     ("C93002A",
      "CHECK THAT DECLARED TASK OBJECTS ARE " &
      "ACTIVATED BEFORE EXECUTION OF THE FIRST " &
      "STATEMENT FOLLOWING THE DECLARATIVE PART");

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (A)

      T : Tt;

   begin -- (A)

      if Global /= 1 then
         Failed
           ("A SIMPLE TASK OBJECT IN A BLOCK WAS " &
            "ACTIVATED TOO LATE - (A)");
      end if;

   end; -- (A)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (B)

      J : Integer;

      function F return Integer is
         A : array (1 .. 1) of Tt;
      begin
         if Global /= 1 then
            Failed
              ("AN ARRAY OF TASK OBJECT IN A FUNCTION " &
               "WAS ACTIVATED TOO LATE - (B)");
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
         type Arr is array (1 .. 1) of Tt;
         type Rt is record
            A : Arr;
         end record;
         R : Rt;
      end P;

      package body P is
      begin
         if Global /= 1 then
            Failed
              ("A RECORD OF TASK OBJECT IN A PACKAGE " &
               "SPECIFICATION WAS ACTIVATED TOO LATE " &
               "- (C1)");
         end if;
      end P;

   begin -- (C1)

      null;

   end; -- (C1)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C2)

      package Q is
         J : Integer;
      private
         type Rt is record
            T : Tt;
         end record;
         R : Rt;
      end Q;

      package body Q is
      begin
         if Global /= 1 then
            Failed
              ("A RECORD OF TASK OBJECT IN A PACKAGE " &
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
         type Rat is record
            A : Arr;
         end record;
      end P;

      package body P is
         Ra : Rat;
      begin
         if Global /= 1 then
            Failed
              ("A RECORD OF ARRAY OF TASK OBJECT IN A " &
               "PACKAGE BODY WAS ACTIVATED " &
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
            T : Tt;
         end record;
         Ar : array (1 .. 1) of Rt;
      begin
         if Global /= 1 then
            Failed
              ("AN ARRAY OF RECORD OF TASK OBJECT IN A " &
               "TASK BODY WAS ACTIVATED TOO LATE - (E)");
         end if;
      end T;

   begin -- (E)

      null;

   end; -- (E)

   --------------------------------------------------

   Result;
end C93002a;

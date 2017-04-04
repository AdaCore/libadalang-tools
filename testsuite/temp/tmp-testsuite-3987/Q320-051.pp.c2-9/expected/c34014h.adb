-- C34014H.ADA

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
--     CHECK THAT A DERIVED SUBPROGRAM IS VISIBLE AND FURTHER DERIVABLE
--     UNDER APPROPRIATE CIRCUMSTANCES.

--     CHECK WHEN THE DERIVED SUBPROGRAM IS IMPLICITLY DECLARED IN THE
--     PRIVATE PART OF A PACKAGE AFTER AN EXPLICIT DECLARATION OF A
--     HOMOGRAPHIC SUBPROGRAM IN THE VISIBLE PART.

-- HISTORY:
--     JRK 09/16/87  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34014h is

   package P is
      type T is range -100 .. 100;
      function F return T;
   end P;
   use P;

   package body P is
      function F return T is
      begin
         return T (Ident_Int (1));
      end F;
   end P;

begin
   Test
     ("C34014H",
      "CHECK THAT A DERIVED SUBPROGRAM IS VISIBLE " &
      "AND FURTHER DERIVABLE UNDER APPROPRIATE " &
      "CIRCUMSTANCES.  CHECK WHEN THE DERIVED " &
      "SUBPROGRAM IS IMPLICITLY DECLARED IN THE " &
      "PRIVATE PART OF A PACKAGE AFTER AN EXPLICIT " &
      "DECLARATION OF A HOMOGRAPHIC SUBPROGRAM IN " &
      "THE VISIBLE PART");

   -----------------------------------------------------------------

   Comment ("NEW SUBPROGRAM DECLARED BY SUBPROGRAM DECLARATION");

   declare

      package Q is
         type Qt is private;
         C2 : constant Qt;
         function F return Qt;
         type Qr1 is record
            C : Qt := F;
         end record;
      private
         type Qt is new T;
         C2 : constant Qt := 2;
         type Qr2 is record
            C : Qt := F;
         end record;
         type Qs is new Qt;
      end Q;
      use Q;

      package body Q is
         function F return Qt is
         begin
            return Qt (Ident_Int (2));
         end F;

         package R is
            X : Qr1;
            Y : Qr2;
            Z : Qs := F;
         end R;
         use R;
      begin
         if X.C /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - SUBPROG " & "DECL - 1");
         end if;

         if Y.C /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - SUBPROG " & "DECL - 2");
         end if;

         if Z /= 2 then
            Failed ("NEW SUBPROGRAM NOT DERIVED - SUBPROG " & "DECL - 1");
         end if;
      end Q;

      package R is
         Y : Qt := F;
         type Rt is new Qt;
         Z : Rt := F;
      end R;
      use R;

   begin
      if Y /= C2 then
         Failed ("NEW SUBPROGRAM NOT VISIBLE - SUBPROG DECL - 3");
      end if;

      if Z /= Rt (C2) then
         Failed ("NEW SUBPROGRAM NOT DERIVED - SUBPROG DECL - 2");
      end if;
   end;

   -----------------------------------------------------------------

   Comment ("NEW SUBPROGRAM DECLARED BY RENAMING");

   declare

      package Q is
         type Qt is private;
         C2 : constant Qt;
         function G return Qt;
         function F return Qt renames G;
         type Qr1 is record
            C : Qt := F;
         end record;
      private
         type Qt is new T;
         C2 : constant Qt := 2;
         type Qr2 is record
            C : Qt := F;
         end record;
         type Qs is new Qt;
      end Q;
      use Q;

      package body Q is
         function G return Qt is
         begin
            return Qt (Ident_Int (2));
         end G;

         package R is
            X : Qr1;
            Y : Qr2;
            Z : Qs := F;
         end R;
         use R;
      begin
         if X.C /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - RENAMING - " & "1");
         end if;

         if Y.C /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - RENAMING - " & "2");
         end if;

         if Z /= 2 then
            Failed ("NEW SUBPROGRAM NOT DERIVED - RENAMING - " & "1");
         end if;
      end Q;

      package R is
         Y : Qt := F;
         type Rt is new Qt;
         Z : Rt := F;
      end R;
      use R;

   begin
      if Y /= C2 then
         Failed ("NEW SUBPROGRAM NOT VISIBLE - RENAMING - 3");
      end if;

      if Z /= Rt (C2) then
         Failed ("NEW SUBPROGRAM NOT DERIVED - RENAMING - 2");
      end if;
   end;

   -----------------------------------------------------------------

   Result;
end C34014h;

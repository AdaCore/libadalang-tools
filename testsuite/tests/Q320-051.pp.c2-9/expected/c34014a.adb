-- C34014A.ADA

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
--     VISIBLE PART OF A PACKAGE AND A HOMOGRAPHIC SUBPROGRAM IS LATER
--     DECLARED EXPLICITLY IN THE SAME VISIBLE PART.

-- HISTORY:
--     JRK 09/08/87  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34014a is

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
     ("C34014A",
      "CHECK THAT A DERIVED SUBPROGRAM IS VISIBLE " &
      "AND FURTHER DERIVABLE UNDER APPROPRIATE " &
      "CIRCUMSTANCES.  CHECK WHEN THE DERIVED " &
      "SUBPROGRAM IS IMPLICITLY DECLARED IN THE " &
      "VISIBLE PART OF A PACKAGE AND A HOMOGRAPHIC " &
      "SUBPROGRAM IS LATER DECLARED EXPLICITLY IN " & "THE SAME VISIBLE PART");

   -----------------------------------------------------------------

   Comment ("NEW SUBPROGRAM DECLARED BY SUBPROGRAM DECLARATION");

   declare

      package Q is
         type Qt is new T;
         X : Qt := F;
         function F return Qt;
         type Qr is record
            C : Qt := F;
         end record;
      private
         type Qs is new Qt;
      end Q;
      use Q;

      package body Q is
         function F return Qt is
         begin
            return Qt (Ident_Int (2));
         end F;

         package R is
            Y : Qr;
            Z : Qs := F;
         end R;
         use R;
      begin
         if X /= 1 then
            Failed ("OLD SUBPROGRAM NOT VISIBLE - SUBPROG " & "DECL");
         end if;

         if Y.C /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - SUBPROG " & "DECL - 1");
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
      if Y /= 2 then
         Failed ("NEW SUBPROGRAM NOT VISIBLE - SUBPROG DECL - 2");
      end if;

      if Z /= 2 then
         Failed ("NEW SUBPROGRAM NOT DERIVED - SUBPROG DECL - 2");
      end if;
   end;

   -----------------------------------------------------------------

   Comment ("NEW SUBPROGRAM DECLARED BY RENAMING");

   declare

      package Q is
         type Qt is new T;
         X : Qt := F;
         function G return Qt;
         function F return Qt renames G;
         type Qr is record
            C : Qt := F;
         end record;
      private
         type Qs is new Qt;
      end Q;
      use Q;

      package body Q is
         function G return Qt is
         begin
            return Qt (Ident_Int (2));
         end G;

         package R is
            Y : Qr;
            Z : Qs := F;
         end R;
         use R;
      begin
         if X /= 1 then
            Failed ("OLD SUBPROGRAM NOT VISIBLE - RENAMING");
         end if;

         if Y.C /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - RENAMING - " & "1");
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
      if Y /= 2 then
         Failed ("NEW SUBPROGRAM NOT VISIBLE - RENAMING - 2");
      end if;

      if Z /= 2 then
         Failed ("NEW SUBPROGRAM NOT DERIVED - RENAMING - 2");
      end if;
   end;

   -----------------------------------------------------------------

   Comment ("NEW SUBPROGRAM DECLARED BY GENERIC INSTANTIATION");

   declare

      generic
         type T is range <>;
      function G return T;

      function G return T is
      begin
         return T (Ident_Int (2));
      end G;

      package Q is
         type Qt is new T;
         X : Qt := F;
         function F is new G (Qt);
         W : Qt := F;
      private
         type Qs is new Qt;
         Z : Qs := F;
      end Q;
      use Q;

      package body Q is
      begin
         if X /= 1 then
            Failed ("OLD SUBPROGRAM NOT VISIBLE - " & "INSTANTIATION");
         end if;

         if W /= 2 then
            Failed ("NEW SUBPROGRAM NOT VISIBLE - " & "INSTANTIATION - 1");
         end if;

         if Z /= 2 then
            Failed ("NEW SUBPROGRAM NOT DERIVED - " & "INSTANTIATION - 1");
         end if;
      end Q;

      package R is
         Y : Qt := F;
         type Rt is new Qt;
         Z : Rt := F;
      end R;
      use R;

   begin
      if Y /= 2 then
         Failed ("NEW SUBPROGRAM NOT VISIBLE - INSTANTIATION - " & "2");
      end if;

      if Z /= 2 then
         Failed ("NEW SUBPROGRAM NOT DERIVED - INSTANTIATION - " & "2");
      end if;
   end;

   -----------------------------------------------------------------

   Result;
end C34014a;

-- C34014R.ADA

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
--     CHECK THAT A DERIVED OPERATOR IS VISIBLE AND FURTHER DERIVABLE
--     UNDER APPROPRIATE CIRCUMSTANCES.

--     CHECK WHEN THE DERIVED OPERATOR IS IMPLICITLY DECLARED IN THE
--     VISIBLE PART OF A PACKAGE AND A HOMOGRAPHIC OPERATOR IS LATER
--     DECLARED EXPLICITLY IN THE PACKAGE BODY.

-- HISTORY:
--     JRK 09/22/87  CREATED ORIGINAL TEST.
--     GJD 11/15/95  REMOVED ADA 83 INCOMPATIBILITIES.
--     PWN 04/11/96  Restored subtests in Ada95 legal format.

with Report; use Report;

procedure C34014r is

   package P is
      type T is range -100 .. 100;
      function "+" (X : T) return T;
   end P;
   use P;

   package body P is
      function "+" (X : T) return T is
      begin
         return X + T (Ident_Int (1));
      end "+";
   end P;

begin
   Test
     ("C34014R",
      "CHECK THAT A DERIVED OPERATOR IS VISIBLE " &
      "AND FURTHER DERIVABLE UNDER APPROPRIATE " &
      "CIRCUMSTANCES.  CHECK WHEN THE DERIVED " &
      "OPERATOR IS IMPLICITLY DECLARED IN THE " &
      "VISIBLE PART OF A PACKAGE AND A HOMOGRAPHIC " &
      "OPERATOR IS LATER DECLARED EXPLICITLY IN " & "THE PACKAGE BODY");

   -----------------------------------------------------------------

   Comment ("NEW OPERATOR DECLARED BY SUBPROGRAM DECLARATION");

   declare

      package Q is
         type Qt is new T;
         X : Qt := +0;
      end Q;
      use Q;

      package body Q is
         function "+" (Y : Qt) return Qt;
         type Qr is record
            C : Qt := +0;
         end record;
         type Qs is new Qt;

         function "+" (Y : Qt) return Qt is
         begin
            return Y + Qt (Ident_Int (2));
         end "+";

         package R is
            Y : Qr;
            Z : Qs := +0;
         end R;
         use R;
      begin
         if X /= 1 then
            Failed ("OLD OPERATOR NOT VISIBLE - SUBPROG " & "DECL - 1");
         end if;

         if Y.C /= 2 then
            Failed ("NEW OPERATOR NOT VISIBLE - SUBPROG " & "DECL");
         end if;

         if Z /= 2 then
            Failed ("OLD OPERATOR NOT DERIVED - SUBPROG " & "DECL - 1");
         end if;
      end Q;

      package R is
         Y : Qt := +0;
         type Rt is new Qt;
         Z : Rt := +0;
      end R;
      use R;

   begin
      if Y /= 1 then
         Failed ("OLD OPERATOR NOT VISIBLE - SUBPROG DECL - 2");
      end if;

      if Z /= 1 then
         Failed ("OLD OPERATOR NOT DERIVED - SUBPROG DECL - 2");
      end if;
   end;

   -----------------------------------------------------------------

   Comment ("NEW OPERATOR DECLARED BY RENAMING");

   declare

      package Q is
         type Qt is new T;
         X : Qt := +0;
      end Q;
      use Q;

      package body Q is
         function G (X : Qt) return Qt;
         function "+" (Y : Qt) return Qt renames G;
         type Qr is record
            C : Qt := +0;
         end record;
         type Qs is new Qt;

         function G (X : Qt) return Qt is
         begin
            return X + Qt (Ident_Int (2));
         end G;

         package R is
            Y : Qr;
            Z : Qs := +0;
         end R;
         use R;
      begin
         if X /= 1 then
            Failed ("OLD OPERATOR NOT VISIBLE - RENAMING - " & "1");
         end if;

         if Y.C /= 2 then
            Failed ("NEW OPERATOR NOT VISIBLE - RENAMING");
         end if;

         if Z /= 2 then
            Failed ("OLD OPERATOR NOT DERIVED - RENAMING - " & "1");
         end if;
      end Q;

      package R is
         Y : Qt := +0;
         type Rt is new Qt;
         Z : Rt := +0;
      end R;
      use R;

   begin
      if Y /= 1 then
         Failed ("OLD OPERATOR NOT VISIBLE - RENAMING - 2");
      end if;

      if Z /= 1 then
         Failed ("OLD OPERATOR NOT DERIVED - RENAMING - 2");
      end if;
   end;

   -----------------------------------------------------------------

   Comment ("NEW OPERATOR DECLARED BY GENERIC INSTANTIATION");

   declare

      generic
         type T is range <>;
      function G (Y : T) return T;

      function G (Y : T) return T is
      begin
         return Y + T (Ident_Int (2));
      end G;

      package Q is
         type Qt is new T;
         X : Qt := +0;
      end Q;
      use Q;

      package body Q is
         function "+" is new G (Qt);
         W : Qt := +0;
         type Qs is new Qt;
         Z : Qs := +0;
      begin
         if X /= 1 then
            Failed ("OLD OPERATOR NOT VISIBLE - " & "INSTANTIATION - 1");
         end if;

         if W /= 2 then
            Failed ("NEW OPERATOR NOT VISIBLE - " & "INSTANTIATION");
         end if;

         if Z /= 2 then
            Failed ("OLD OPERATOR NOT DERIVED - " & "INSTANTIATION - 1");
         end if;
      end Q;

      package R is
         Y : Qt := +0;
         type Rt is new Qt;
         Z : Rt := +0;
      end R;
      use R;

   begin
      if Y /= 1 then
         Failed ("OLD OPERATOR NOT VISIBLE - INSTANTIATION - " & "2");
      end if;

      if Z /= 1 then
         Failed ("OLD OPERATOR NOT DERIVED - INSTANTIATION - " & "2");
      end if;
   end;

   -----------------------------------------------------------------

   Result;
end C34014r;

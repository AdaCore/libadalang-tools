-- CC1005B.ADA

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
--     CHECK THAT A GENERIC UNIT'S IDENTIFIER CAN BE USED IN ITS
--     FORMAL PART:
--
--     (A) AS THE SELECTOR IN AN EXPANDED NAME TO DENOTE AN ENTITY IN THE
--         VISIBLE PART OF A PACKAGE, OR TO DENOTE AN ENTITY IMMEDIATELY
--         ENCLOSED IN A CONSTRUCT OTHER THAN THE CONSTRUCT IMMEDIATELY
--         ENCLOSING THE GENERIC UNIT.
--
--     (B) AS A SELECTOR TO DENOTE A COMPONENT OF A RECORD OBJECT,
--         AS THE NAME OF A RECORD OR DISCRIMINANT COMPONENT IN A RECORD
--         AGGREGATE, AND AS THE NAME OF A FORMAL PARAMETER IN A
--         FUNCTION CALL.

-- HISTORY:
--     BCB 08/03/88  CREATED ORIGINAL TEST.
--     JRL 03/20/92  DELETED TEST IN BLOCK STATEMENT; CONSOLIDATED
--                   WITH CC1005C.

with Report; use Report;

procedure Cc1005b is

   S : Integer := Ident_Int (0);

   package Cc1005b is
      I : Integer;
      S : Integer := Ident_Int (5);
      generic
         S : Integer := Ident_Int (10);
         V : Integer := Standard.Cc1005b.S;
         W : Integer := Standard.Cc1005b.Cc1005b.S;
      function Cc1005b return Integer;
   end Cc1005b;

   package body Cc1005b is
      function Cc1005b return Integer is
      begin
         if not Equal (V, 0) then
            Failed ("WRONG VALUE OF S USED IN ASSIGNMENT OF V");
         end if;

         if not Equal (W, 5) then
            Failed ("WRONG VALUE OF S USED IN ASSIGNMENT OF W");
         end if;

         return 0;
      end Cc1005b;

      function New_Cc is new Cc1005b;

   begin
      Test
        ("CC1005B",
         "CHECK THAT A GENERIC UNIT'S IDENTIFIER " &
         "CAN BE USED IN ITS FORMAL PART: AS THE " &
         "SELECTOR IN AN EXPANDED NAME TO DENOTE " &
         "AN ENTITY IN THE VISIBLE PART OF A " &
         "PACKAGE, OR TO DENOTE AN ENTITY " &
         "IMMEDIATELY ENCLOSED IN A CONSTRUCT " &
         "OTHER THAN THE CONSTRUCT IMMEDIATELY " &
         "ENCLOSING THE GENERIC UNIT; AND AS A " &
         "SELECTOR TO DENOTE A COMPONENT OF A " &
         "RECORD OBJECT, AS THE NAME OF A RECORD " &
         "OR DISCRIMINANT COMPONENT IN A RECORD " &
         "AGGREGATE, AND AS THE NAME OF A FORMAL " &
         "PARAMETER IN A FUNCTION CALL");

      I := New_Cc;
   end Cc1005b;

   function F (P : Integer) return Integer is
   begin
      return P;
   end F;

begin

   Block1 : declare
      type Rec is record
         P : Integer := Ident_Int (0);
      end record;

      type Rec2 (P : Integer) is record
         null;
      end record;

      R : Rec;

      J : Integer;

      generic
         V : Integer := R.P;
         X : Rec := (P => Ident_Int (10));
         Y : Rec2 := (P => Ident_Int (15));
         Z : Integer := F (P => Ident_Int (20));
      function P return Integer;

      function P return Integer is
      begin
         if not Equal (V, 0) then
            Failed ("WRONG VALUE OF P USED IN ASSIGNMENT " & "OF V");
         end if;

         if not Equal (X.P, 10) then
            Failed ("WRONG VALUE USED IN ASSIGNMENT OF X.P");
         end if;

         if not Equal (Y.P, 15) then
            Failed ("WRONG VALUE USED IN ASSIGNMENT OF Y.P");
         end if;

         if not Equal (Z, 20) then
            Failed ("WRONG VALUE OF P USED IN ASSIGNMENT " & "OF Z");
         end if;

         return 0;
      end P;

      function New_P is new P;
   begin
      J := New_P;
   end Block1;

   Result;
end Cc1005b;

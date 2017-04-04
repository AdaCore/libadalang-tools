-- CC3240A.ADA

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
--      CHECK THAT A FORMAL PRIVATE AND LIMITED PRIVATE TYPE DENOTES ITS
--      ACTUAL PARAMETER, AND OPERATIONS OF THE FORMAL TYPE ARE
--      IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL TYPE
--      WHEN THE FORMAL TYPE IS A TYPE WITH DISCRIMINANTS.

-- HISTORY:
--      RJW 10/13/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cc3240a is

begin
   Test
     ("CC3240A",
      "CHECK THAT A FORMAL PRIVATE OR LIMITED " &
      "PRIVATE TYPE DENOTES ITS ACTUAL PARAMETER AND " &
      "OPERATIONS OF THE FORMAL TYPE ARE IDENTIFIED " &
      "WITH CORRESPONDING OPERATIONS OF THE ACTUAL " &
      "TYPE, WHEN THE FORMAL TYPE IS A TYPE " &
      "WITH DISCRIMINANTS");

   declare

      generic
         type T (A : Integer) is private;
      package P is
         subtype S is T;
         Tx : T (5);
      end P;

      type Rec (L : Integer) is record
         A : Integer;
      end record;

      package P1 is new P (Rec);
      use P1;

   begin
      Tx := (L => 5, A => 7);
      if not (Tx in Rec) then
         Failed ("MEMBERSHIP TEST - PRIVATE");
      end if;

      if Tx.A /= 7 or Tx.L /= 5 then
         Failed ("SELECTED COMPONENTS - PRIVATE");
      end if;

      if S (Tx) /= Rec (Tx) then
         Failed ("EXPLICIT CONVERSION - PRIVATE");
      end if;

      if not Tx'Constrained then
         Failed ("'CONSTRAINED - PRIVATE");
      end if;
   end;

   declare
      type Rec (L : Integer) is record
         A : Integer;
      end record;

      generic
         type T (A : Integer) is limited private;
         Tx : in out T;
      package Lp is
         subtype S is T;
      end Lp;

      R : Rec (5) := (5, 7);

      package body Lp is
      begin
         if (Tx in S) /= (R in Rec) then
            Failed ("MEMBERSHIP TEST - LIMITED PRIVATE");
         end if;

         if Tx.A /= 5 then
            Failed ("SELECTED COMPONENTS - LIMITED PRIVATE");
         end if;

         if (S (Tx) in S) /= (Rec (R) in Rec) then
            Failed ("EXPLICIT CONVERSION - LIMITED PRIVATE");
         end if;

         if not Tx'Constrained then
            Failed ("'CONSTRAINED - LIMITED PRIVATE");
         end if;
      end Lp;

      package P1 is new Lp (Rec, R);
      use P1;
   begin
      null;
   end;

   Result;
end Cc3240a;

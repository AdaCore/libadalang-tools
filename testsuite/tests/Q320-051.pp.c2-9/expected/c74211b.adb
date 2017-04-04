-- C74211B.ADA

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
-- CHECK THAT IMPLICITLY DECLARED INEQUALITY WHICH ACCOMPANIES AN EXPLICIT
-- DECLARATION OF EQUALITY HIDES OTHER IMPLICITLY DECLARED HOMOGRAPHS, AND
-- THAT DERIVED INEQUALITY HIDES PREDEFINED INEQUALITY.

-- DSJ 4/29/83
-- JBG 9/23/83

with Report;
procedure C74211b is

   use Report;

begin

   Test
     ("C74211B",
      "CHECK THAT HIDING OF IMPLICITLY DECLARED " &
      "OPERATORS AND DERIVED SUBPROGRAMS IS DONE " &
      "CORRECTLY REGARDLESS OF ORDER OF DECL'S");

   declare

      package P1 is
         type Lt1 is limited private;
         function "=" (L, R : Lt1) return Boolean;
         function Lt1_Value_2 return Lt1;
         function Lt1_Value_4 return Lt1;
         type Lt2 is limited private;
      private
         type Lt1 is range 1 .. 10;
         type Lt2 is range 1 .. 10;
      end P1;

      use P1;

      package P2 is
         type Lt3 is limited private;
         type Lt4 is new Lt1;
      private
         function "=" (L, R : Lt3) return Boolean;
         type Lt3 is new Lt1;
      end P2;

      use P2;

      package body P1 is
         A, B : constant Lt1 := 4;
         C, D : constant Lt2 := 6;

         function "=" (L, R : Lt1) return Boolean is
         begin
            return Integer (L) /= Integer (R);
         end "=";

         function Lt1_Value_2 return Lt1 is
         begin
            return 2;
         end Lt1_Value_2;

         function Lt1_Value_4 return Lt1 is
         begin
            return 4;
         end Lt1_Value_4;

      begin
         if A = B then
            Failed
              ("PREDEFINED EQUALITY NOT HIDDEN BY " &
               "EXPLICIT DECLARATION - LT1");
         end if;

         if C /= D then
            Failed ("WRONG PREDEFINED OPERATION - T2");
         end if;
      end P1;

      package body P2 is
         function U return Lt3 is
         begin
            return Lt1_Value_2;
         end U;

         function V return Lt3 is
         begin
            return Lt1_Value_4;
         end V;

         function W return Lt4 is
         begin
            return Lt1_Value_2;
         end W;

         function X return Lt4 is
         begin
            return Lt1_Value_4;
         end X;

         function "=" (L, R : Lt3) return Boolean is
         begin
            return not (Lt1 (L) = Lt1 (R));
         end "=";

      begin
         if not (U /= V) then
            Failed
              ("DERIVED SUBPROGRAM NOT HIDDEN BY " &
               "IMPLICITLY DECLARED INEQUALITY " &
               "FROM EXPLICITLY DECLARED EQUALITY");
         end if;

         if not (Lt3 (W) = U) then
            Failed
              ("DERIVED SUBPROGRAM NOT HIDDEN BY " &
               "EXPLICIT DECLARATION - '=' ");
         end if;

         if W /= X then
            Failed
              ("PREDEFINED OPERATOR NOT HIDDEN BY " &
               "DERIVED SUBPROGRAM - '/=' ");
         end if;

         if not (X = W) then
            Failed
              ("PREDEFINED OPERATOR NOT HIDDEN BY " &
               "DERIVED SUBPROGRAM - '=' ");
         end if;

      end P2;

   begin

      null;

   end;

   Result;

end C74211b;

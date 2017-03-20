-- C96004A.ADA

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
--     CHECK THAT THE PRE-DEFINED SUBTYPES FROM THE PACKAGE CALENDAR,
--     NAMELY YEAR_NUMBER, MONTH_NUMBER, DAY_NUMBER, AND DAY_DURATION,
--     HAVE THE CORRECT RANGE CONSTRAINTS. SUBTESTS ARE:
--       (A) YEAR_NUMBER.
--       (B) MONTH_NUMBER.
--       (C) DAY_NUMBER.
--       (D) DAY_DURATION.

-- HISTORY:
--     CPP 08/15/84  CREATED ORIGINAL TEST.
--     JET 01/06/88  UPDATED HEADER FORMAT AND ADDED CODE TO PREVENT
--                   OPTIMIZATION.
--     RLB 12/18/06  Changed so that the test will work for any
--                   allowed Year_Number'Last.
--     RLB 03/14/07  Changed so that Year_Number'Last must be 2399.

with Calendar; use Calendar;
with Report;   use Report;
procedure C96004a is

begin
   Test
     ("C96004A",
      "CHECK THAT PRE-DEFINED SUBTYPES FROM THE " &
      "CALENDAR PACKAGE HAVE CORRECT RANGE CONSTRAINTS");

   ---------------------------------------------

   declare   -- (A)

      Yr : Year_Number;

   begin     -- (A)

      begin
         Yr := 1_900;
         Failed ("EXCEPTION NOT RAISED - (A)1");
         if not Equal (Yr, Yr) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (A)1");
      end;

      begin
         Yr := 84;
         Failed ("EXCEPTION NOT RAISED - (A)2");
         if not Equal (Yr, Yr) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (A)2");
      end;

      begin
         Yr := 2_099;
         if not Equal (Yr, Yr) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when others =>
            Failed ("OK CASE RAISED EXCEPTION ON 2099 - (A)");
      end;

      begin
         Yr := Ident_Int (2_100);
         if not Equal (Yr, Yr) then
            Comment ("NO EXCEPTION RAISED");
         end if;
         begin
            Yr := 2_399;
            if not Equal (Yr, Yr) then
               Comment ("NO EXCEPTION RAISED");
            end if;

         exception
            when others =>
               Failed ("OK CASE RAISED EXCEPTION ON 2399 - (A)");
         end;
         begin
            Yr := Ident_Int (2_400);
            if not Equal (Yr, Yr) then
               Comment ("NO EXCEPTION RAISED");
            end if;
            Failed ("EXCEPTION NOT RAISED - (A)3");
         exception
            when Constraint_Error =>
               null;
         end;
      exception
         when Constraint_Error =>
            Failed
              ("Upper Bound of Year_Number is appropriate" & " for Ada 95");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (A)3");
      end;

   end; -- (A)

   ---------------------------------------------

   declare   -- (B)

      Mo : Month_Number;

   begin     -- (B)

      begin
         Mo := Ident_Int (0);
         Failed ("EXCEPTION NOT RAISED - (B)1");
         if not Equal (Mo, Mo) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (B)1");
      end;

      begin
         Mo := 12;
         if not Equal (Mo, Mo) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when others =>
            Failed ("OK CASE RAISED EXCEPTION ON 12 - (B)");
      end;

      begin
         Mo := 13;
         Failed ("EXCEPTION NOT RAISED - (B)2");
         if not Equal (Mo, Mo) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (B)2");
      end;

   end; -- (B)

   ---------------------------------------------

   declare   -- (C)

      Dy : Day_Number;

   begin     -- (C)

      begin
         Dy := 0;
         Failed ("EXCEPTION NOT RAISED - (C)1");
         if not Equal (Dy, Dy) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (C)1");
      end;

      begin
         Dy := Ident_Int (32);
         Failed ("EXCEPTION NOT RAISED - (C)2");
         if not Equal (Dy, Dy) then
            Comment ("NO EXCEPTION RAISED");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (C)2");
      end;

   end; -- (C)

   ---------------------------------------------

   declare   -- (D)

      Segment : Day_Duration;

      function Check_Ok (X : Day_Duration) return Boolean is
         I : Integer := Integer (X);
      begin
         return Equal (I, I);
      end Check_Ok;

   begin     -- (D)

      begin
         Segment := 86_400.0;
         if Check_Ok (Segment - 86_000.0) then
            Comment ("NO EXCEPTION RAISED (D1)");
         else
            Comment ("NO EXCEPTION RAISED (D2)");
         end if;

      exception
         when others =>
            Failed ("OK CASE RAISED EXCEPTION ON 86_400 - (D)");
      end;

      begin
         Segment := -4.0;
         Failed ("EXCEPTION NOT RAISED - (D)1");
         if not Equal (Integer (Segment), Integer (Segment)) then
            Comment ("NO EXCEPTION RAISED (D3)");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (D)1");
      end;

      begin
         Segment := 86_401.00;
         if Check_Ok (Segment - 86_000.0) then
            Failed ("NO EXCEPTION RAISED (D4)");
         else
            Failed ("NO EXCEPTION RAISED (D5)");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (D)2");
      end;

   end; -- (D)

   ---------------------------------------------

   Result;
end C96004a;

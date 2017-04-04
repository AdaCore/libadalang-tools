-- C96006A.ADA

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
-- CHECK THAT FOR THE PACKAGE CALENDAR, THE RELATIONAL OPERATORS WORK
-- CORRECTLY FOR OPERANDS OF TYPE TIME AND TYPE DURATION. PARTICULARLY,
--   (A) RELATIONS BASED ON YEARS.
--   (B) RELATIONS BASED ON MONTH.
--   (C) RELATIONS BASED ON SECONDS.
--   (D) RELATIONS AT EXTREMES OF THE PERMITTED RANGE OF TIME.

-- CPP 8/16/84

with Calendar; use Calendar;
with Report;   use Report;
procedure C96006a is

begin
   Test
     ("C96006A",
      "CHECK THAT RELATIONAL OPERATORS WORK " &
      "CORRECTLY IN THE PACKAGE CALENDAR");

   --------------------------------------------

   declare   -- (A)
      -- RELATIONS BASED ON YEARS.
      Now, Later : Time;
   begin     -- (A)
      Now   := Time_Of (1_984, 8, 12, 500.0);
      Later := Time_Of (1_985, 8, 12, 500.0);

      if Now < Later then
         Comment ("< OPERATOR OK - (A)");
      else
         Failed ("< OPERATOR INCORRECT - (A)");
      end if;

      if Now <= Later then
         Comment ("<= OPERATOR OK - (A)");
      else
         Failed ("<= OPERATOR INCORRECT - (A)");
      end if;

      if Now <= Now then
         Comment ("<= OPERATOR OK - (A)2");
      else
         Failed ("<= OPERATOR INCORRECT - (A)2");
      end if;

      if Later > Now then
         Comment ("> OPERATOR OK - (A)");
      else
         Failed ("> OPERATOR INCORRECT - (A)");
      end if;

      if Later >= Now then
         Comment (">= OPERATOR OK - (A)");
      else
         Failed (">= OPERATOR INCORRECT - (A)");
      end if;

      if Later >= Later then
         Comment (">= OPERATOR OK - (A)2");
      else
         Failed (">= OPERATOR INCORRECT - (A)2");
      end if;

   end; -- (A)

   --------------------------------------------

   declare   -- (B)
      -- RELATIONS BASED ON MONTH.
      Now, Later : Time;
   begin     -- (B)
      Now   := Time_Of (1_984, 8, 12, 500.0);
      Later := Time_Of (1_984, 9, 12, 500.0);

      if Now < Later then
         Comment ("< OPERATOR OK - (B)");
      else
         Failed ("< OPERATOR INCORRECT - (B)");
      end if;

      if Now <= Later then
         Comment ("<= OPERATOR OK - (B)");
      else
         Failed ("<= OPERATOR INCORRECT - (B)");
      end if;

      if Now <= Now then
         Comment ("<= OPERATOR OK - (B)2");
      else
         Failed ("<= OPERATOR INCORRECT - (B)2");
      end if;

      if Later > Now then
         Comment ("> OPERATOR OK - (B)");
      else
         Failed ("> OPERATOR INCORRECT - (B)");
      end if;

      if Later >= Now then
         Comment (">= OPERATOR OK - (B)");
      else
         Failed (">= OPERATOR INCORRECT - (B)");
      end if;

      if Later >= Later then
         Comment (">= OPERATOR OK - (B)2");
      else
         Failed (">= OPERATOR INCORRECT - (B)2");
      end if;

      if Now = Now then
         Comment ("= OPERATOR OK - (B)");
      else
         Failed ("= OPERATOR INCORRECT - (B)");
      end if;

      if Later /= Now then
         Comment ("/= OPERATOR OK - (B)");
      else
         Failed ("/= OPERATOR INCORRECT - (B)");
      end if;

   end; -- (B)

   --------------------------------------------

   declare   -- (C)
      -- RELATIONS BASED ON SECONDS.
      Now, Later : Time;
      Increment  : Duration := 99.9;
   begin     -- (C)
      Now   := Time_Of (1_984, 8, 12, 500.0);
      Later := Now + Increment;

      if Now < Later then
         Comment ("< OPERATOR OK - (C)");
      else
         Failed ("< OPERATOR INCORRECT - (C)");
      end if;

      if Now <= Later then
         Comment ("<= OPERATOR OK - (C)");
      else
         Failed ("<= OPERATOR INCORRECT - (C)");
      end if;

      if Now <= Now then
         Comment ("<= OPERATOR OK - (C)2");
      else
         Failed ("<= OPERATOR INCORRECT - (C)2");
      end if;

      if Later > Now then
         Comment ("> OPERATOR OK - (C)");
      else
         Failed ("> OPERATOR INCORRECT - (C)");
      end if;

      if Later >= Now then
         Comment (">= OPERATOR OK - (C)");
      else
         Failed (">= OPERATOR INCORRECT - (C)");
      end if;

      if Later >= Later then
         Comment (">= OPERATOR OK - (C)2");
      else
         Failed (">= OPERATOR INCORRECT - (C)2");
      end if;

      if Later = Later then
         Comment ("= OPERATOR OK - (C)");
      else
         Failed ("= OPERATOR INCORRECT - (C)");
      end if;

      if Now /= Later then
         Comment ("/= OPERATOR OK - (C)");
      else
         Failed ("/= OPERATOR INCORRECT - (C)");
      end if;

      if Now < Now then
         Failed ("NOW < NOW INCORRECT - (C)");
      elsif Now /= Now then
         Failed ("NOW = NOW INCORRECT - (C)");
      elsif Later < Now then
         Failed ("LATER < NOW INCORRECT - (C)");
      elsif Later <= Now then
         Failed ("LATER <= NOW INCORRECT - (C)");
      elsif Later = Now then
         Failed ("NOW = LATER INCORRECT - (C)");
      elsif Now > Later then
         Failed ("NOW > LATER INCORRECT - (C)");
      elsif Now > Now then
         Failed ("NOW > NOW INCORRECT - (C)");
      elsif Now >= Later then
         Failed ("NOW >= LATER INCORRECT - (C)");
      elsif Now = Later then
         Failed ("NOW = LATER INCORRECT - (C)");
      end if;

   end; -- (C)

   --------------------------------------------

   declare   -- (D)

      Now, Way_Back_Then : Time;

   begin     -- (D)

      Now           := Time_Of (2_099, 12, 31);
      Way_Back_Then := Time_Of (1_901, 1, 1);

      begin
         if Now < Way_Back_Then then
            Failed ("TEST < AT EXTREMES INCORRECT - (D)");
         end if;
      exception
         when others =>
            Failed ("< AT EXTREMES RAISED EXCEPTION - (D)");
      end;

      begin
         if Now <= Way_Back_Then then
            Failed ("TEST <= AT EXTREMES INCORRECT - (D)");
         end if;
      exception
         when others =>
            Failed ("<= AT EXTREMES RAISED EXCEPTION - (D)");
      end;

      begin
         if Way_Back_Then > Now then
            Failed ("TEST > AT EXTREMES INCORRECT - (D)");
         end if;
      exception
         when others =>
            Failed ("> AT EXTREMES RAISED EXCEPTION - (D)");
      end;

      begin
         if Way_Back_Then >= Now then
            Failed ("TEST >= AT EXTREMES INCORRECT - (D)");
         end if;
      exception
         when others =>
            Failed (">= AT EXTREMES RAISED EXCEPTION - (D)");
      end;

      begin
         if Way_Back_Then /= Way_Back_Then then
            Failed ("TEST /= AT EXTREMES INCORRECT - (D)");
         end if;
      exception
         when others =>
            Failed ("/= AT EXTREMES RAISED EXCEPTION - (D)");
      end;

      begin
         if Now = Way_Back_Then then
            Failed ("TEST = AT EXTREMES INCORRECT - (D)");
         end if;
      exception
         when others =>
            Failed ("= AT EXTREMES RAISED EXCEPTION - (D)");
      end;

   end; -- (D)

   --------------------------------------------

   Result;
end C96006a;

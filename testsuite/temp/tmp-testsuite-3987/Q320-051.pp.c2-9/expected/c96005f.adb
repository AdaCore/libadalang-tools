-- C96005F.ADA

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
-- CHECK THAT PACKAGE CALENDAR + AND - FUNCTIONS WORK PROPERLY,
-- ESPECIALLY WITH VALUES AT MIDNIGHT.

-- GOM 02/18/85
-- JWC 05/14/85

with Report;   use Report;
with Calendar; use Calendar;

procedure C96005f is

   Curr_Day1 : constant Time := Time_Of (1_984, 1, 1, 0.0);
   Curr_Day2 : constant Time := Time_Of (1_984, 1, 1, Day_Duration'Last);
   Curr_Day3 : constant Time := Time_Of (1_984, 1, 1, 10_000.0);

   Tomorrow1 : constant Time := Time_Of (1_984, 1, 2, 0.0);
   Tomorrow2 : constant Time := Time_Of (1_984, 1, 2, Day_Duration'Last);
   Tomorrow3 : constant Time := Time_Of (1_984, 1, 2, 10_000.0);

   Yesterday1 : constant Time := Time_Of (1_983, 12, 31, 0.0);
   Yesterday2 : constant Time := Time_Of (1_983, 12, 31, Day_Duration'Last);
   Yesterday3 : constant Time := Time_Of (1_983, 12, 31, 10_000.0);

begin
   Test ("C96005F", "CHECKING PACKAGE CALENDAR + AND - FUNCTIONS");

   -- CHECK IF ADDING ONE DAY TO 'CURR_DAY#' TIMES YIELDS
   -- TIMES EQUAL TO 'TOMORROW'.

   if (Curr_Day1 + Day_Duration'Last) /= Tomorrow1 then
      Failed ("FAILURE IN ADDING 1 DAY TO 'CURR_DAY1'");
   end if;

   if (Curr_Day2 + Day_Duration'Last) /= Tomorrow2 then
      Failed ("FAILURE IN ADDING 1 DAY TO 'CURR_DAY2'");
   end if;

   if (Curr_Day3 + Day_Duration'Last) /= Tomorrow3 then
      Failed ("FAILURE IN ADDING 1 DAY TO 'CURR_DAY3'");
   end if;

   if (Curr_Day1 + Day_Duration'Last) /= Curr_Day2 then
      Failed ("'CURR_DAY1' + 1 /= 'CURR_DAY2'");
   end if;

   -- CHECK IF SUBTRACTING ONE DAY FROM 'CURR_DAY#' TIMES YIELDS
   -- TIMES EQUAL TO 'YESTERDAY'.

   if (Curr_Day1 - Day_Duration'Last) /= Yesterday1 then
      Failed ("FAILURE IN SUBTRACTING 1 DAY FROM 'CURR_DAY1'");
   end if;

   if (Curr_Day2 - Day_Duration'Last) /= Yesterday2 then
      Failed ("FAILURE IN SUBTRACTING 1 DAY FROM 'CURR_DAY2'");
   end if;

   if (Curr_Day3 - Day_Duration'Last) /= Yesterday3 then
      Failed ("FAILURE IN SUBTRACTING 1 DAY FROM 'CURR_DAY3'");
   end if;

   if (Curr_Day2 - Day_Duration'Last) /= Curr_Day1 then
      Failed ("'CURR_DAY2' - 1 /= 'CURR_DAY1'");
   end if;

   Result;
end C96005f;

-- CE3202A.ADA

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
-- CHECK THAT CURRENT_INPUT AND CURRENT_OUTPUT INITIALLY CORRESPOND TO STANDARD
-- FILES.

-- ABW  8/25/82
-- SPS  11/9/82
-- JBG  3/17/83
-- JBG 5/8/84

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3202a is

begin

   Test
     ("CE3202A",
      "CHECK THAT CURRENT_INPUT AND " &
      "CURRENT_OUTPUT INITIALLY " &
      "CORRESPOND TO STANDARD FILES");

   if Name (Current_Input) /= Name (Standard_Input) then
      Failed ("CURRENT_INPUT INCORRECT - NAME");
   end if;

   if Name (Current_Output) /= Name (Standard_Output) then
      Failed ("CURRENT_OUTPUT INCORRECT - NAME");
   end if;

   Result;

end Ce3202a;

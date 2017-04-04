-- C95034B.ADA

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
-- CHECK THAT A CALLING TASK REMAINS SUSPENDED UNTIL THE ACCEPT STATEMENT
-- RECEIVING THIS ENTRY CALL HAS COMPLETED THE EXECUTION OF ITS SEQUENCE
-- OF STATEMENTS.

-- WEI  3/ 4/82
-- JWC 6/28/85 RENAMED FROM C950CBA-B.ADA

with Report; use Report;
procedure C95034b is

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   procedure Pspy_Numb (Digt : in Arg) is
   begin
      Spynumb := 10 * Spynumb + Digt;
   end Pspy_Numb;

   task T1 is
      entry E1;
   end T1;

   task body T1 is
   begin
      accept E1 do
         Pspy_Numb (1);
         delay 1.0;
         Pspy_Numb (2);
      end E1;
   end T1;

   task T2 is
      entry Bye;
   end T2;

   task body T2 is
   begin
      T1.E1;
      Pspy_Numb (3);
      accept Bye;
   end T2;

begin

   Test
     ("C95034B",
      "TASK SUSPENSION UNTIL COMPLETION OF ACCEPT " & "STATEMENT");

   T2.Bye;

   if Spynumb /= 123 then
      Failed ("ERROR DURING TASK EXECUTION");
      Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
   end if;

   Result;

end C95034b;

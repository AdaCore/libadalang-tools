-- C95033A.ADA

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
-- CHECK THAT - IN THE CASE OF AN ENTRY FAMILY - EXECUTION OF AN ACCEPT
-- STATEMENT STARTS WITH THE EVALUATION OF AN ENTRY INDEX.

-- WEI  3/ 4/82
-- JWC 6/28/85 RENAMED FROM C950BGA-B.ADA

with Report; use Report;
procedure C95033a is

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   procedure Pspy_Numb (Digt : in Arg) is
   begin
      Spynumb := 10 * Spynumb + Digt;
   end Pspy_Numb;

   function Finit_Pos (Digt : in Arg) return Natural is
   begin
      Spynumb := 10 * Spynumb + Digt;
      return Digt;
   end Finit_Pos;

   task T1 is
      entry E1 (Natural range 1 .. 2);
      entry Bye;
   end T1;

   task body T1 is
   begin
      accept E1 (Finit_Pos (1)) do
         Pspy_Numb (2);
      end E1;
      accept Bye;
   end T1;

begin
   Test ("C95033A", "EVALUATION OF ENTRY INDEX");

   T1.E1 (1);
   T1.Bye;
   if Spynumb /= 12 then
      Failed ("ENTRY INDEX NOT EVALUATED FIRST");
      Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
   end if;

   Result;

end C95033a;

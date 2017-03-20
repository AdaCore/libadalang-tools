-- C58004C.ADA

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
-- CHECK THAT THE RETURN STATEMENT WORKS FOR RECURSIVE SUBPROGRAMS,
--    BOTH FUNCTIONS AND PROCEDURES.

-- DCB 2/8/80
-- SPS 3/7/83
-- JBG 5/17/83

with Report;
procedure C58004c is

   use Report;

   I1, I2 : Integer := 0;   -- INITIAL VALUE IS IMMATERIAL

   procedure Factorialp (Ip1 : in Integer; Ip2 : in out Integer) is

   begin
      if Ip1 = 1 then
         Ip2 := 1;
         return;
      else
         Factorialp (Ip1 - 1, Ip2);
         Ip2 := Ip1 * Ip2;
         return;
      end if;

      Ip2 := 0;

   end Factorialp;

   function Factorialf (If1 : Integer) return Integer is

   begin
      if If1 = 1 then
         return (1);
      end if;

      return (If1 * Factorialf (If1 - 1));

   end Factorialf;

begin
   Test
     ("C58004C",
      "CHECK THAT THE RETURN STATEMENT WORKS FOR" &
      " RECURSIVE FUNCTIONS AND PROCEDURES");

   I1 := Factorialf (5);

   if I1 /= 120 then
      Failed ("RETURN STATEMENT IN RECURSIVE FUNCTION NOT " & "WORKING");
   end if;

   Factorialp (5, I2);

   if I2 = 0 then
      Failed ("RETURN STATEMENT IN RECURSIVE PROCEDURE NOT " & "WORKING");
   elsif I2 /= 120 then
      Failed ("RETURN STMT IN RECURSIVE PROCEDURE NOT WORKING CORRECTLY");
   end if;

   Result;
end C58004c;

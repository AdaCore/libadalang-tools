-- C35A02A.ADA

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
-- CHECK THAT T'DELTA YIELDS CORRECT VALUES FOR SUBTYPE T.

-- RJW 2/27/86

with Report; use Report;

procedure C35a02a is

begin

   Test
     ("C35A02A",
      "CHECK THAT T'DELTA YIELDS CORRECT VALUES " & "FOR SUBTYPE T");

   declare
      D  : constant := 0.125;
      Sd : constant := 1.0;

      type Volt is delta D range 0.0 .. 255.0;
      subtype Rough_Voltage is Volt delta Sd;

      generic
         type Fixed is delta <>;
      function F return Fixed;

      function F return Fixed is
      begin
         return Fixed'Delta;
      end F;

      function Vf is new F (Volt);
      function Rf is new F (Rough_Voltage);

   begin
      if Volt'Delta /= D then
         Failed ("INCORRECT VALUE FOR VOLT'DELTA");
      end if;
      if Rough_Voltage'Delta /= Sd then
         Failed ("INCORRECT VALUE FOR ROUGH_VOLTAGE'DELTA");
      end if;

      if Vf /= D then
         Failed ("INCORRECT VALUE FOR VF");
      end if;
      if Rf /= Sd then
         Failed ("INCORRECT VALUE FOR RF");
      end if;
   end;

   Result;

end C35a02a;

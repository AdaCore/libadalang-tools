-- C4A010B.ADA

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
-- CHECK THAT STATIC UNIVERSAL REAL EXPRESSIONS ARE EVALUATED EXACTLY. IN
-- PARTICULAR, CHECK THAT THE CASCADING USE OF FRACTIONAL VALUES DOES NOT
-- RESULT IN THE LOSS OF PRECISION.

-- RJW 7/31/86

with Report; use Report;
procedure C4a010b is

begin

   Test
     ("C4A010B",
      "CHECK THAT STATIC UNIVERSAL REAL EXPRESSIONS " &
      "ARE EVALUATED EXACTLY.  IN PARTICULAR, CHECK " &
      "THAT THE CASCADING USE OF FRACTIONAL VALUES " &
      "DOES NOT RESULT IN THE LOSS OF PRECISION");

   declare
      B : constant := 2.0 / 3.0;

      X0 : constant := 1.0;
      X1 : constant := X0 + B;
      X2 : constant := X1 + B**2;
      X3 : constant := X2 + B**3;
      X4 : constant := X3 + B**4;
      X5 : constant := X4 + B**5;
      X6 : constant := X5 + B**6;
      X7 : constant := X6 + B**7;
      X8 : constant := X7 + B**8;
      X9 : constant := X8 + B**9;

      Y1 : constant := B**10;
      Y2 : constant := 1.0;
      Y3 : constant := Y1 - Y2;
      Y4 : constant := B;
      Y5 : constant := Y4 - Y2;
      Y6 : constant := Y3 / Y5;

   begin
      if X9 /= 58_025.0 / 19_683.0 then
         Failed ("INCORRECT RESULTS FOR SERIES OF NAMED " & "NUMBERS  - 1");
      end if;

      if Y6 /= 58_025.0 / 19_683.0 then
         Failed ("INCORRECT RESULTS FOR SERIES OF NAMED " & "NUMBERS  - 2");
      end if;

      if X9 /= Y6 then
         Failed ("INCORRECT RESULTS FOR SERIES OF NAMED " & "NUMBERS  - 3");
      end if;

   end;

   Result;
end C4a010b;

-- C87B06A.ADA

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
--     CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
--     FOR EACH INTEGER TYPE, THERE EXISTS AN IMPLICIT CONVERSION THAT
--     CONVERTS A UNIVERSAL INTEGER VALUE INTO THE CORRESPONDING VALUE
--     OF THE INTEGER TYPE. THIS TEST USES LITERALS AS UNIVERSAL INTEGER
--     VALUES.

-- HISTORY:
--     TRH 08/11/82 CREATED ORIGINAL TEST.
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C87b06a is

   type Minor is new Integer range 0 .. 17;
   type Fixed is new Duration;
   type Real is new Float;

   Err : Boolean := False;

   procedure P (X : Boolean) is
   begin
      Err := True;
   end P;
   procedure P (X : Fixed) is
   begin
      Err := True;
   end P;

   procedure P (X : Real) is
   begin
      Err := True;
   end P;

   procedure P (X : Float) is
   begin
      Err := True;
   end P;

   procedure P (X : String) is
   begin
      Err := True;
   end P;

   procedure P (X : Minor) is
   begin
      null;
   end P;

begin
   Test
     ("C87B06A",
      "OVERLOADING RESOLUTION WITH IMPLICIT CONVERSION " &
      "OF UNIVERSAL INTEGER VALUES TO INTEGER VALUES. " &
      "CONVERSIONS TO INTEGER VALUES EXISTS FOR ANY INTEGER TYPE");

   P (2);
   P (2 * 2 + 2);

   if Err then
      Failed
        ("INCORRECT IMPLICIT CONVERSION FROM UNIVERSAL " &
         " INTEGER VALUES TO INTEGER TYPE VALUES");
   end if;

   Result;
end C87b06a;

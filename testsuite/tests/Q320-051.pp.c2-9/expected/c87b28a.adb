-- C87B28A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- THE TYPE OF THE LITERAL "NULL" MUST BE DETERMINED FROM THE FACT
-- THAT "NULL" IS A VALUE OF AN ACCESS TYPE.

-- TRH  13 AUG 82
-- JRK 2/2/84

with Report; use Report;

procedure C87b28a is

   Err : Boolean := False;

   type A2 is access Boolean;
   type A3 is access Integer;
   type A1 is access A2;

   function F return A1 is
   begin
      return new A2;
   end F;

   function F return A2 is
   begin
      Err := True;
      return new Boolean;
   end F;

   function F return A3 is
   begin
      Err := True;
      return (new Integer);
   end F;

begin
   Test ("C87B28A", "OVERLOADING OF THE ACCESS TYPE LITERAL 'NULL'");

   F.all := null;

   if Err then
      Failed ("RESOLUTION INCORRECT FOR THE ACCESS TYPE LITERAL " & "'NULL'");
   end if;

   Result;
end C87b28a;

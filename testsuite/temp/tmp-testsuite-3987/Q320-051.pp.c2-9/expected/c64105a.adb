-- C64105A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED AT THE TIME OF CALL WHEN
--   THE VALUE OF AN ACTUAL OUT SCALAR PARAMETER DOES NOT SATISFY THE
--   RANGE CONSTRAINTS OF THE FORMAL PARAMETER.

-- DAS  1/29/81
-- CPP  8/6/84

with Report;
procedure C64105a is

   use Report;

   subtype Subint1 is Integer range -10 .. 10;
   subtype Subint2 is Integer range -20 .. 20;

   I10 : Subint1 := 10;
   I20 : Subint2 := 20;

   procedure P1 (I : out Subint1) is
   begin
      I := Subint1'First;
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN PROCEDURE P1");
   end P1;

begin

   Test
     ("C64105A",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED" &
      " AT THE TIME OF CALL WHEN THE VALUE OF AN" &
      " ACTUAL OUT SCALAR PARAMETER DOES NOT" &
      " SATISFY THE RANGE CONSTRAINTS OF THE FORMAL" &
      " PARAMETER");

   declare
   begin
      P1 (Subint1 (I20));
      if I20 /= Ident_Int (-10) then
         Failed ("OUT PARAM DID NOT GET CORRECT VALUE - 1");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED ON CALL TO P1 - 1");
   end;

   declare
   begin
      I20 := Ident_Int (20);
      P1 (I20);
      if I20 /= Ident_Int (-10) then
         Failed ("OUT PARAM DID NOT GET CORRECT VALUE - 2");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED ON CALL TO P1 - 2");
   end;

   Result;

end C64105a;

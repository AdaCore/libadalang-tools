-- C43205E.ADA

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
-- CHECK THAT THE BOUNDS OF A POSITIONAL AGGREGATE ARE DETERMINED CORRECTLY.
-- IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY 'FIRST OF THE INDEX
-- SUBTYPE WHEN THE POSITIONAL AGGREGATE IS USED AS:

--   E) THE LEFT OR RIGHT OPERAND OF "&".

-- EG  01/26/84

with Report;

procedure C43205e is

   use Report;

begin

   Test ("C43205E", "CASE E : OPERAND OF &");

   begin

      Case_E : declare

         subtype Ste is Integer range 2 .. 10;

         type Color is (Red, Green, Blue);
         type Te is array (Ste range <>) of Color;

         function Concat1 return Te is
         begin
            return (Red, Green, Blue) & (7 .. 8 => Red);
         end Concat1;

         function Concat2 return Te is
         begin
            return (Ident_Int (4) .. 3 => Red) & (Green, Blue);
         end Concat2;

         function Concat3 return String is
         begin
            return "TEST" & (7 .. 8 => 'X');
         end Concat3;

         function Concat4 return String is
         begin
            return (8 .. 5 => 'A') & "BC";
         end Concat4;

      begin

         if Concat1'First /= Ident_Int (2) then
            Failed ("CASE E1 : LOWER BOUND INCORRECTLY " & "GIVEN BY 'FIRST");
         elsif Concat1'Last /= 6 then
            Failed ("CASE E1 : UPPER BOUND INCORRECTLY " & "GIVEN BY 'LAST");
         elsif Concat1 /= (Red, Green, Blue, Red, Red) then
            Failed ("CASE E1 : INCORRECT VALUES PRODUCED");
         end if;
         if Concat2'First /= Ident_Int (2) then
            Failed ("CASE E2 : LOWER BOUND INCORRECTLY " & "GIVEN BY 'FIRST");
         elsif Concat2'Last /= 3 then
            Failed ("CASE E2 : UPPER BOUND INCORRECTLY " & "GIVEN BY 'LAST");
         elsif Concat2 /= (Green, Blue) then
            Failed ("CASE E2 : INCORRECT VALUES PRODUCED");
         end if;
         if Concat3'First /= Ident_Int (1) then
            Failed ("CASE E3 : LOWER BOUND INCORRECTLY " & "GIVEN BY 'FIRST");
         elsif Concat3'Last /= 6 then
            Failed ("CASE E3 : UPPER BOUND INCORRECTLY " & "GIVEN BY 'LAST");
         elsif Concat3 /= "TESTXX" then
            Failed ("CASE E3 : INCORRECT VALUES PRODUCED");
         end if;
         if Concat4'First /= Ident_Int (1) then
            Failed ("CASE E4 : LOWER BOUND INCORRECTLY " & "GIVEN BY 'FIRST");
         elsif Concat4'Last /= 2 then
            Failed ("CASE E4 : UPPER BOUND INCORRECTLY " & "GIVEN BY 'LAST");
         elsif Concat4 /= "BC" then
            Failed ("CASE E4 : INCORRECT VALUES PRODUCED");
         end if;

      end Case_E;

   end;

   Result;

end C43205e;

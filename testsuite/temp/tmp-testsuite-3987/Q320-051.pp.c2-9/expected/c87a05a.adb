-- C87A05A.ADA

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
-- CHECK THAT FUNCTION CALLS AND INDEXED COMPONENT EXPRESSIONS CAN BE
-- DISTINGUISHED BY THE RULES OF OVERLOADING RESOLUTION.
--
-- PART 1 : CORRECT RESOLUTION IS INDEXED COMPONENT EXPRESSION

-- TRH  13 JULY 82
-- DSJ  09 JUNE 83

with Report; use Report;

procedure C87a05a is

   Ok : Boolean := True;
   type Vector is array (1 .. 5) of Boolean;

   procedure P (Arg : Boolean) is          -- THIS IS CORRECT P
   begin
      Ok := Arg;
   end P;

   procedure P (Arg : Character) is
   begin
      Ok := False;
   end P;

   function Y return Vector is             -- THIS IS CORRECT Y
   begin
      return (Vector'Range => True);
   end Y;

   function Y (Arg : Integer) return Float is
   begin
      Ok := False;
      return 0.0;
   end Y;

   function Y (Arg : Character) return Character is
   begin
      Ok := False;
      return 'A';
   end Y;

   function Y (Arg : Float) return Float is
   begin
      Ok := False;
      return 0.0;
   end Y;

   function Y return Boolean is
   begin
      Ok := False;
      return False;
   end Y;

   function Y (Arg : Character := 'A') return Boolean is
   begin
      Ok := False;
      return False;
   end Y;

   function Z return Integer is            -- THIS IS CORRECT Z
   begin
      return 3;
   end Z;

   function Z return Float is
   begin
      Ok := False;
      return 3.0;
   end Z;

begin
   Test
     ("C87A05A",
      "OVERLOADING RESOLUTION FOR DISTINGUISHING " &
      "FUNCTION CALLS FROM INDEXED COMPONENTS WHERE INDEXED " &
      "COMPONENTS ARE CORRECT");

   P (Y (Z));

   if not Ok then
      Failed ("RESOLUTION INCORRECT");
   end if;

   Result;
end C87a05a;

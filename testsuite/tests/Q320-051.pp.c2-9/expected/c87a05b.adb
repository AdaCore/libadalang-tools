-- C87A05B.ADA

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
-- PART 2 : CORRECT RESOLUTION IS FUNCTION CALL

-- TRH  15 JULY 82
-- DSJ  09 JUNE 83

with Report; use Report;

procedure C87a05b is

   Ok : Boolean := True;
   type Vector is array (1 .. 5) of Boolean;

   procedure P (Arg : Character := 'A') is
   begin
      Ok := False;
   end P;

   procedure P is
   begin
      Ok := False;
   end P;

   procedure P (Arg : Integer) is               -- THIS IS CORRECT P
   begin
      Ok := (Arg = 1);
   end P;

   function Y return Vector is
   begin
      Ok := False;
      return (Vector'Range => True);
   end Y;

   function Y return Character is
   begin
      Ok := False;
      return 'A';
   end Y;

   function Y (Arg : Float) return Float is
   begin
      Ok := False;
      return 0.0;
   end Y;

   function Y (Arg : Character) return Integer is
   begin
      Ok := False;
      return 0;
   end Y;

   function Y (Arg : Float) return Integer is   -- THIS IS CORRECT Y
   begin
      return 1;
   end Y;

   function Z return Integer is
   begin
      Ok := False;
      return 3;
   end Z;

   function Z return Float is                   -- THIS IS CORRECT Z
   begin
      return 3.0;
   end Z;

begin
   Test
     ("C87A05B",
      "OVERLOADING RESOLUTION FOR DISTINGUISHING " &
      "FUNCTION CALLS FROM INDEXED COMPONENTS WHERE CORRECT " &
      "RESOLUTION IS FUNCTION CALL");

   P (Y (Z));

   if not Ok then
      Failed ("RESOLUTION INCORRECT");
   end if;

   Result;
end C87a05b;

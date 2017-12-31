-- CD2D11A.ADA

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
--     CHECK THAT IF A SMALL SPECIFICATION IS GIVEN FOR A
--     FIXED POINT TYPE, THEN ARITHMETIC OPERATIONS ON VALUES OF THE
--     TYPE ARE NOT AFFECTED BY THE REPRESENTATION CLAUSE.

-- HISTORY:
--     BCB 09/01/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;
procedure Cd2d11a is

   Basic_Small : constant := 2.0**(-4);

   type Basic_Type is delta 2.0**(-4) range -4.0 .. 4.0;

   type Check_Type is delta 1.0 range -4.0 .. 4.0;

   for Check_Type'Small use Basic_Small;

   Cneg1 : Check_Type := -3.5;
   Cneg2 : Check_Type := Check_Type (-1.0 / 3.0);
   Cpos1 : Check_Type := Check_Type (4.0 / 6.0);
   Cpos2 : Check_Type := 3.5;
   Czero : Check_Type;

   type Array_Type is array (0 .. 3) of Check_Type;
   Charray : Array_Type :=
     (-3.5, Check_Type (-1.0 / 3.0), Check_Type (4.0 / 6.0), 3.5);

   type Rec_Type is record
      Compn1 : Check_Type := -3.5;
      Compn2 : Check_Type := Check_Type (-1.0 / 3.0);
      Compp1 : Check_Type := Check_Type (4.0 / 6.0);
      Compp2 : Check_Type := 3.5;
   end record;

   Chrec : Rec_Type;

   function Ident (Fx : Check_Type) return Check_Type is
   begin
      if Equal (3, 3) then
         return Fx;
      else
         return 0.0;
      end if;
   end Ident;

   procedure Proc (N1_In, P1_In :        Check_Type;
      N2_Inout, P2_Inout        : in out Check_Type; Czout : out Check_Type)
   is
   begin

      if Ident (N1_In) + P1_In not in -2.875 .. -2.812_5 or
        P2_Inout - Ident (P1_In) not in 2.812_5 .. 2.875 then
         Failed ("INCORRECT RESULTS FOR " & "BINARY ADDING OPERATORS - 1");
      end if;

      if +Ident (N2_Inout) not in -0.375 .. -0.312_5 or
        Ident (-P1_In) not in -0.687_5 .. -0.625 then
         Failed ("INCORRECT RESULTS FOR " & "UNARY ADDING OPERATORS - 1");
      end if;

      if Check_Type (N1_In * Ident (P1_In)) not in -2.437_5 .. -2.187_5 or
        Check_Type (Ident (N2_Inout) / P2_Inout) not in -0.125 .. -0.062_5 then
         Failed ("INCORRECT RESULTS FOR " & "MULTIPLYING OPERATORS - 1");
      end if;

      if abs Ident (N2_Inout) not in 0.312_5 .. 0.375 or
        Ident (abs P1_In) not in 0.625 .. 0.687_5 then
         Failed ("INCORRECT RESULTS FOR " & "ABSOLUTE VALUE OPERATORS - 1");
      end if;

      Czout := 0.0;

   end Proc;

begin
   Test
     ("CD2D11A",
      "CHECK THAT IF A SMALL SPECIFICATION IS " &
      "GIVEN FOR AN FIXED POINT TYPE, THEN " &
      "ARITHMETIC OPERATIONS ON VALUES OF THE " &
      "TYPE ARE NOT AFFECTED BY THE REPRESENTATION " & "CLAUSE");

   Proc (Cneg1, Cpos1, Cneg2, Cpos2, Czero);

   if Ident (Czero) /= 0.0 then
      Failed ("INCORRECT VALUE FOR OUT PARAMETER");
   end if;

   if Ident (Cneg1) + Cpos1 not in -2.875 .. -2.812_5 or
     Cpos2 - Ident (Cpos1) not in 2.812_5 .. 2.875 then
      Failed ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 2");
   end if;

   if +Ident (Cneg2) not in -0.375 .. -0.312_5 or
     Ident (-Cpos1) not in -0.687_5 .. -0.625 then
      Failed ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 2");
   end if;

   if Check_Type (Cneg1 * Ident (Cpos1)) not in -2.437_5 .. -2.187_5 or
     Check_Type (Ident (Cneg2) / Cpos2) not in -0.125 .. -0.062_5 then
      Failed ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 2");
   end if;

   if abs Ident (Cneg2) not in 0.312_5 .. 0.375 or
     Ident (abs Cpos1) not in 0.625 .. 0.687_5 then
      Failed ("INCORRECT RESULTS FOR ABSOLUTE VALUE " & "OPERATORS - 2");
   end if;

   if Ident (Cpos1) not in 0.625 .. 0.687_5 or Cneg2 in -0.25 .. 0.0 or
     Ident (Cneg2) in -1.0 .. -0.437_5 then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 2");
   end if;

   if Ident (Charray (0)) + Charray (2) not in -2.875 .. -2.812_5 or
     Charray (3) - Ident (Charray (2)) not in 2.812_5 .. 2.875 then
      Failed ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 3");
   end if;

   if +Ident (Charray (1)) not in -0.375 .. -0.312_5 or
     Ident (-Charray (2)) not in -0.687_5 .. -0.625 then
      Failed ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 3");
   end if;

   if Check_Type (Charray (0) * Ident (Charray (2))) not in
       -2.437_5 .. -2.187_5 or
     Check_Type (Ident (Charray (1)) / Charray (3)) not in -0.125 .. -0.062_5
   then
      Failed ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 3");
   end if;

   if abs Ident (Charray (1)) not in 0.312_5 .. 0.375 or
     Ident (abs Charray (2)) not in 0.625 .. 0.687_5 then
      Failed ("INCORRECT RESULTS FOR ABSOLUTE VALUE " & "OPERATORS - 3");
   end if;

   if Ident (Charray (2)) not in 0.625 .. 0.687_5 or
     Charray (1) in -0.25 .. 0.0 or Ident (Charray (1)) in -1.0 .. -0.437_5
   then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 3");
   end if;

   if Ident (Chrec.Compn1) + Chrec.Compp1 not in -2.875 .. -2.812_5 or
     Chrec.Compp2 - Ident (Chrec.Compp1) not in 2.812_5 .. 2.875 then
      Failed ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 4");
   end if;

   if +Ident (Chrec.Compn2) not in -0.375 .. -0.312_5 or
     Ident (-Chrec.Compp1) not in -0.687_5 .. -0.625 then
      Failed ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 4");
   end if;

   if Check_Type (Chrec.Compn1 * Ident (Chrec.Compp1)) not in
       -2.437_5 .. -2.187_5 or
     Check_Type (Ident (Chrec.Compn2) / Chrec.Compp2) not in -0.125 .. -0.062_5
   then
      Failed ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 4");
   end if;

   if abs Ident (Chrec.Compn2) not in 0.312_5 .. 0.375 or
     Ident (abs Chrec.Compp1) not in 0.625 .. 0.687_5 then
      Failed ("INCORRECT RESULTS FOR ABSOLUTE VALUE " & "OPERATORS - 4");
   end if;

   if Ident (Chrec.Compp1) not in 0.625 .. 0.687_5 or
     Chrec.Compn2 in -0.25 .. 0.0 or Ident (Chrec.Compn2) in -1.0 .. -0.437_5
   then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 4");
   end if;

   Result;
end Cd2d11a;

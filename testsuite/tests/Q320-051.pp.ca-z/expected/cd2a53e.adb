-- CD2A53E.ADA

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
--     CHECK THAT WHEN SIZE AND SMALL SPECIFICATIONS ARE GIVEN FOR A
--     FIXED POINT TYPE, THEN OPERATIONS ON VALUES OF SUCH A TYPE
--     ARE NOT AFFECTED BY THE REPRESENTATION CLAUSE WHEN THE TYPE
--     IS PASSED AS A GENERIC ACTUAL PARAMETER.

-- HISTORY:
--     BCB 08/24/87  CREATED ORIGINAL TEST.
--     DHH 04/12/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND CHANGED
--                   OPERATORS ON 'SIZE TESTS.
--     WMC 04/01/92  ELIMINATED TEST REDUNDANCIES.
--     MRM 07/16/92  FIX ALIGNMENT OF BLOCK BODY
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cd2a53e is

   Basic_Size  : constant := Integer'Size / 2;
   Basic_Small : constant := 2.0**(-4);
   B           : Boolean;

   type Check_Type is delta 1.0 range -4.0 .. 4.0;
   for Check_Type'Small use Basic_Small;
   for Check_Type'Size use Basic_Size;

begin

   Test
     ("CD2A53E",
      "CHECK THAT WHEN SIZE AND SMALL SPECIFICATIONS " &
      "ARE GIVEN FOR A FIXED POINT TYPE, THEN " &
      "OPERATIONS ON VALUES OF SUCH A TYPE ARE NOT " &
      "AFFECTED BY THE REPRESENTATION CLAUSE WHEN " &
      "THE TYPE IS PASSED AS A GENERIC ACTUAL " &
      "PARAMETER");

   declare

      generic

         type Fixed_Element is delta <>;

      function Func return Boolean;

      function Func return Boolean is

         Zero : constant := 0.0;

         type Basic_Type is delta 2.0**(-4) range -4.0 .. 4.0;

         Cneg1 : Fixed_Element := -3.5;
         Cneg2 : Fixed_Element := Fixed_Element (-1.0 / 3.0);
         Cpos1 : Fixed_Element := Fixed_Element (4.0 / 6.0);
         Cpos2 : Fixed_Element := 3.5;
         Czero : Fixed_Element;

         type Array_Type is array (0 .. 3) of Fixed_Element;
         Charray : Array_Type :=
           (-3.5, Fixed_Element (-1.0 / 3.0), Fixed_Element (4.0 / 6.0), 3.5);

         type Rec_Type is record
            Compf : Fixed_Element := -3.5;
            Compn : Fixed_Element := Fixed_Element (-1.0 / 3.0);
            Compp : Fixed_Element := Fixed_Element (4.0 / 6.0);
            Compl : Fixed_Element := 3.5;
         end record;

         Chrec : Rec_Type;

         function Ident (Fx : Fixed_Element) return Fixed_Element is
         begin
            if Equal (3, 3) then
               return Fx;
            else
               return 0.0;
            end if;
         end Ident;

         procedure Proc
           (Cn1in, Cp1in       :        Fixed_Element;
            Cn2inout, Cp2inout : in out Fixed_Element;
            Czout              :    out Fixed_Element)
         is
         begin

            if +Ident (Cn2inout) not in -0.375 .. -0.312_5 or
              Ident (-Cp1in) not in -0.687_5 .. -0.625
            then
               Failed
                 ("INCORRECT RESULTS FOR " & "UNARY ADDING OPERATORS - 1");
            end if;

            if abs Ident (Cn2inout) not in 0.312_5 .. 0.375 or
              Ident (abs Cp1in) not in 0.625 .. 0.687_5
            then
               Failed
                 ("INCORRECT RESULTS FOR " & "ABSOLUTE VALUE OPERATORS - 1");
            end if;

            Czout := 0.0;

         end Proc;

      begin -- FUNC

         Proc (Cneg1, Cpos1, Cneg2, Cpos2, Czero);

         if Ident (Czero) /= Zero then
            Failed ("INCORRECT VALUE FOR OUT PARAMETER");
         end if;

         if Fixed_Element'Last < Ident (3.937_5) then
            Failed ("INCORRECT VALUE FOR FIXED_ELEMENT'LAST");
         end if;

         if Fixed_Element'Size /= Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR FIXED_ELEMENT'SIZE");
         end if;

         if Fixed_Element'Small /= Basic_Small then
            Failed ("INCORRECT VALUE FOR FIXED_ELEMENT'SMALL");
         end if;

         if Fixed_Element'Aft /= 1 then
            Failed ("INCORRECT VALUE FOR FIXED_ELEMENT'AFT");
         end if;

         if Cneg1'Size < Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR CNEG1'SIZE");
         end if;

         if Ident (Cneg1) + Cpos1 not in -2.875 .. -2.812_5 or
           Cpos2 - Ident (Cpos1) not in 2.812_5 .. 2.875
         then
            Failed ("INCORRECT RESULTS FOR BINARY ADDING " & "OPERATORS - 2");
         end if;

         if Fixed_Element (Cneg1 * Ident (Cpos1)) not in
             -2.437_5 .. -2.187_5 or
           Fixed_Element (Ident (Cneg2) / Cpos2) not in -0.125 .. -0.062_5
         then
            Failed ("INCORRECT RESULTS FOR MULTIPLYING " & "OPERATORS - 2");
         end if;

         if Ident (Cpos1) not in 0.625 .. 0.687_5 or
           Cneg2 in -0.25 .. 0.0 or
           Ident (Cneg2) in -1.0 .. -0.437_5
         then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 2");
         end if;

         if Charray (1)'Size < Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR CHARRAY(1)'SIZE");
         end if;

         if +Ident (Charray (1)) not in -0.375 .. -0.312_5 or
           Ident (-Charray (2)) not in -0.687_5 .. -0.625
         then
            Failed ("INCORRECT RESULTS FOR UNARY ADDING " & "OPERATORS - 3");
         end if;

         if abs Ident (Charray (1)) not in 0.312_5 .. 0.375 or
           Ident (abs Charray (2)) not in 0.625 .. 0.687_5
         then
            Failed ("INCORRECT RESULTS FOR ABSOLUTE VALUE " & "OPERATORS - 3");
         end if;

         if Ident (Charray (2)) not in 0.625 .. 0.687_5 or
           Charray (1) in -0.25 .. 0.0 or
           Ident (Charray (1)) in -1.0 .. -0.437_5
         then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 3");
         end if;

         if Chrec.Compp'Size < Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR CHREC.COMPP'SIZE");
         end if;

         if Ident (Chrec.Compf) + Chrec.Compp not in -2.875 .. -2.812_5 or
           Chrec.Compl - Ident (Chrec.Compp) not in 2.812_5 .. 2.875
         then
            Failed ("INCORRECT RESULTS FOR BINARY ADDING " & "OPERATORS - 4");
         end if;

         if Fixed_Element (Chrec.Compf * Ident (Chrec.Compp)) not in
             -2.437_5 .. -2.187_5 or
           Fixed_Element (Ident (Chrec.Compn) / Chrec.Compl) not in
             -0.125 .. -0.062_5
         then
            Failed ("INCORRECT RESULTS FOR MULTIPLYING " & "OPERATORS - 4");
         end if;

         if Ident (Chrec.Compp) not in 0.625 .. 0.687_5 or
           Chrec.Compn in -0.25 .. 0.0 or
           Ident (Chrec.Compn) in -1.0 .. -0.437_5
         then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 4");
         end if;

         return True;

      end Func;

      function Newfunc is new Func (Check_Type);
   begin
      B := Newfunc;
   end;

   Result;

end Cd2a53e;

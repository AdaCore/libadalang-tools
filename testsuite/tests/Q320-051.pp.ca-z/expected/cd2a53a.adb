-- CD2A53A.ADA

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
--     FIXED POINT TYPE, THEN OPERATIONS ON VALUES OF SUCH A TYPE ARE
--     NOT AFFECTED BY THE REPRESENTATION CLAUSE.

-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C)
--          and which support decimal small values:
--          The test must compile, bind, execute, report PASSED, and
--          complete normally.
--
--      For other implementations:
--          This test may produce at least one error message at compilation,
--          and the error message is associated with one of the items marked:
--             -- N/A => ERROR.
--          The test will be recorded as Not_Applicable.
--          Otherwise, the test must execute and report PASSED.
--
--      All other behaviors are FAILING.
--
-- HISTORY:
--     BCB 08/24/87  CREATED ORIGINAL TEST.
--     DHH 04/12/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   OPERATORS ON 'SIZE TESTS, AND CHANGED 'SIZE CLAUSE
--                   SO THAT IT IS NOT A POWER OF TWO.
--     WMC 04/01/92  ELIMINATED TEST REDUNDANCIES.
--     RLB 11/24/98  Added Ada 95 applicability criteria.

with Report; use Report;
procedure Cd2a53a is
   Basic_Size  : constant := 15;
   Basic_Small : constant := 0.01;

   Zero : constant := 0.0;

   type Check_Type is delta 1.0 range -4.0 .. 4.0;

   for Check_Type'Small use Basic_Small;   -- N/A => ERROR.
   for Check_Type'Size use Basic_Size;     -- N/A => ERROR.

   Cneg1 : Check_Type := -2.7;
   Cneg2 : Check_Type := Check_Type (-1.0 / 3.0);
   Cpos1 : Check_Type := Check_Type (4.0 / 6.0);
   Cpos2 : Check_Type := 2.7;
   Czero : Check_Type;

   type Array_Type is array (0 .. 3) of Check_Type;
   Charray : Array_Type :=
     (-2.7, Check_Type (-1.0 / 3.0), Check_Type (4.0 / 6.0), 2.7);

   type Rec_Type is record
      Compf : Check_Type := -2.7;
      Compn : Check_Type := Check_Type (-1.0 / 3.0);
      Compp : Check_Type := Check_Type (4.0 / 6.0);
      Compl : Check_Type := 2.7;
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

   procedure Proc (Cn1in, Cp1in :        Check_Type;
      Cn2inout, Cp2inout        : in out Check_Type; Czout : out Check_Type)
   is
   begin

      if Ident (Cn1in) + Cp1in not in -2.04 .. -2.03 or
        Cp2inout - Ident (Cp1in) not in 2.03 .. 2.04 then
         Failed ("INCORRECT RESULTS FOR " & "BINARY ADDING OPERATORS - 1");
      end if;

      if Check_Type (Cn1in * Ident (Cp1in)) not in -1.81 .. -1.78 or
        Check_Type (Ident (Cn2inout) / Cp2inout) not in -0.13 .. -0.12 then
         Failed ("INCORRECT RESULTS FOR " & "MULTIPLYING OPERATORS - 1");
      end if;

      if Ident (Cp1in) not in 0.66 .. 0.670 or Cn2inout in -0.32 .. 0.0 or
        Ident (Cn2inout) in -1.0 .. -0.35 then
         Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 1");
      end if;

      Czout := 0.0;

   end Proc;

begin
   Test
     ("CD2A53A",
      "CHECK THAT WHEN SIZE AND SMALL SPECIFICATIONS " &
      "ARE GIVEN FOR A FIXED POINT TYPE, THEN " &
      "OPERATIONS ON VALUES OF SUCH A TYPE ARE NOT " &
      "AFFECTED BY THE REPRESENTATION CLAUSE");

   Proc (Cneg1, Cpos1, Cneg2, Cpos2, Czero);

   if Cneg1'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CNEG1'SIZE");
   end if;

   if Ident (Czero) /= Zero then
      Failed ("INCORRECT VALUE FOR OUT PARAMETER");
   end if;

   if Check_Type'First > Ident (-3.99) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'FIRST");
   end if;

   if Check_Type'Size /= Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
   end if;

   if Check_Type'Small /= Basic_Small then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'SMALL");
   end if;

   if Check_Type'Fore /= 2 then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'FORE");
   end if;

   if +Ident (Cneg2) not in -0.34 .. -0.33 or
     Ident (-Cpos1) not in -0.67 .. -0.66 then
      Failed ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 2");
   end if;

   if abs Ident (Cneg2) not in 0.33 .. 0.34 or
     Ident (abs Cpos1) not in 0.66 .. 0.670 then
      Failed ("INCORRECT RESULTS FOR ABSOLUTE VALUE " & "OPERATORS - 2");
   end if;

   if Charray (1)'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CHARRAY(1)'SIZE");
   end if;

   if Ident (Charray (0)) + Charray (2) not in -2.04 .. -2.03 or
     Charray (3) - Ident (Charray (2)) not in 2.03 .. 2.04 then
      Failed ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 3");
   end if;

   if Check_Type (Charray (0) * Ident (Charray (2))) not in -1.81 .. -1.78 or
     Check_Type (Ident (Charray (1)) / Charray (3)) not in -0.13 .. -0.12 then
      Failed ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 3");
   end if;

   if Ident (Charray (2)) not in 0.66 .. 0.670 or
     Charray (1) in -0.32 .. 0.0 or Ident (Charray (1)) in -1.0 .. -0.35 then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 3");
   end if;

   if Chrec.Compp'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CHREC.COMPP'SIZE");
   end if;

   if +Ident (Chrec.Compn) not in -0.34 .. -0.33 or
     Ident (-Chrec.Compp) not in -0.67 .. -0.66 then
      Failed ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 4");
   end if;

   if abs Ident (Chrec.Compn) not in 0.33 .. 0.34 or
     Ident (abs Chrec.Compp) not in 0.66 .. 0.670 then
      Failed ("INCORRECT RESULTS FOR ABSOLUTE VALUE " & "OPERATORS - 4");
   end if;

   if Ident (Chrec.Compp) not in 0.66 .. 0.670 or
     Chrec.Compn in -0.32 .. 0.0 or Ident (Chrec.Compn) in -1.0 .. -0.35 then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 4");
   end if;

   Result;

end Cd2a53a;

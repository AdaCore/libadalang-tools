-- C49024A.ADA

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
-- CHECK THAT A FUNCTION CALL CAN APPEAR IN A STATIC EXPRESSION IF THE
-- FUNCTION NAME DENOTES A PREDEFINED OPERATOR AND HAS THE FORM OF AN
-- OPERATOR SYMBOL OR AN EXPANDED NAME WHOSE SELECTOR IS AN OPERATOR
-- SYMBOL.

-- L.BROWN  10/02/86

with Report; use Report;
procedure C49024a is

   package P is
      type Ty is new Integer;
   end P;

   Con1 : constant P.Ty := 3;
   Con2 : constant P.Ty := 4;
   type Int1 is range 1 .. P."+" (Con1, Con2);
   Con3 : constant := 5;
   Con4 : constant := 7;
   type Flt is digits "-" (Con4, Con3);
   type Fix1 is delta 1.0 range 0.0 .. 25.0;
   Con5 : constant := 3.0;
   Con6 : constant := 6.0;
   type Fix2 is delta 1.0 range 0.0 .. "/" (Con6, Con5);
   type Enum is (Red, Blue, Green, Black);
   Con7     : constant Boolean := True;
   Con8     : constant Enum    := Blue;
   Cas_Int1 : constant         := 10;
   Cas_Int2 : constant         := 2;
   Obj1     : Integer          := 10;
   Cas_Bol  : Boolean          := True;
   Con9     : constant Enum    := Black;
   Con10    : constant Fix1    := 2.0;
   Con11    : constant Fix1    := 10.0;
   type Fix3 is delta "+" (Con10) range 0.0 .. 20.0;
   type Int2 is range 0 .. "ABS" ("-" (Con4));
   Con12 : constant Character := 'D';
   Con13 : constant Character := 'B';
   Con14 : constant Boolean   := False;
   Con15 : constant           := 10;

begin

   Test
     ("C49024A",
      "A FUNCTION CALL CAN BE IN A STATIC EXPRESSION " &
      "IF THE FUNCTION NAME DENOTES A PREDEFINED " &
      "OPERATOR AND HAS THE FORM OF AN OPERATOR SYMBOL");

   case Cas_Bol is
      when ("NOT" (Con7)) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 1");
      when ("/=" (Con8, Con9)) =>
         Obj1 := 2;
   end case;
   Cas_Bol := True;

   case Cas_Bol is
      when ("*" (Con3, Con4) = Cas_Int1) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 2");
      when ("ABS" (Con15) = Cas_Int1) =>
         Obj1 := 3;
   end case;
   Cas_Bol := True;

   case Cas_Bol is
      when ("<" (Con11, Con10)) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 3");
      when ("<=" (Con13, Con12)) =>
         Obj1 := 4;
   end case;
   Cas_Bol := True;

   case Cas_Bol is
      when ("REM" (Con4, Con3) = Cas_Int2) =>
         Obj1 := 5;
      when ("**" (Con3, Con4) = Cas_Int2) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 4");
   end case;

   case Cas_Bol is
      when (P.">" (Con1, Con2)) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 5");
      when ("OR" (Con7, Con14)) =>
         Obj1 := 6;
   end case;
   Cas_Bol := True;

   case Cas_Bol is
      when ("MOD" (Con4, Con3) = Cas_Int2) =>
         Obj1 := 7;
      when ("ABS" (Con4) = Cas_Int2) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 6");
   end case;

   case Cas_Bol is
      when ("AND" (Con7, Con14)) =>
         Failed ("INCORRECT VALUE RETURNED FOR STATIC " & "OPERATORS 7");
      when (">=" (Con12, Con13)) =>
         Obj1 := 9;
   end case;

   Result;

end C49024a;

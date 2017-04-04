-- C49025A.ADA

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
-- CHECK THAT CERTAIN ATTRIBUTES CAN BE USED IN STATIC EXPRESSIONS SUCH AS:
-- 'SUCC, 'PRED, 'POS, 'VAL, 'AFT, 'DELTA, 'DIGITS, 'FIRST,
--'FORE, 'LAST, 'MACHINE_EMAX, 'MACHINE_EMIN, 'MACHINE_MANTISSA,
--'MACHINE_OVERFLOWS, 'MACHINE_RADIX, 'MACHINE_ROUNDS, 'SIZE, 'SMALL, 'WIDTH.

-- L.BROWN  10/07/86
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C49025a is

   type Enum is (Red, Blue, Green, Black);
   type Fix is delta 0.125 range 0.0 .. 20.0;
   type Flt is digits 3 range 0.0 .. 25.0;
   type Int is range 1 .. 100;
   type Tint1 is range 1 .. Enum'Pos (Blue);
   type Tflt is digits Fix'Aft range 0.0 .. 10.0;
   type Tfix2 is delta Fix'Delta range 0.0 .. 5.0;
   type Tflt1 is digits Flt'Digits;
   type Itn is range 0 .. Int'First;
   type Tint2 is range 1 .. Fix'Fore;
   type Tflt3 is digits 3 range 5.0 .. Flt'Last;
   Con3 : constant := Flt'Machine_Emax;
   type Tint3 is range Flt'Machine_Emin .. 1;
   Con4 : constant := Flt'Machine_Mantissa;
   type Tint4 is range 1 .. Flt'Machine_Radix;
   Con6 : constant := Int'Size;
   type Tfix5 is delta 0.125 range 0.0 .. Fix'Small;
   type Tint6 is range 1 .. Enum'Width;
   Obj1    : Integer := 1;
   Cas_Obj : Boolean := True;

begin

   Test
     ("C49025A",
      "CHECK THAT CERTAIN ATTRIBUTES CAN " & "BE USED IN STATIC EXPRESSIONS.");

   case Cas_Obj is
      when (Enum'Pred (Blue) = Enum'(Red)) =>
         Obj1 := 2;
      when others =>
         Failed ("INCORRECT VALUE RETURNED FOR ATTRIBUTE 1");
   end case;
   Cas_Obj := True;

   case Cas_Obj is
      when (Enum'Succ (Red) = Enum'(Blue)) =>
         Obj1 := 3;
      when others =>
         Failed ("INCORRECT VALUE RETURNED FOR ATTRIBUTE 2");
   end case;
   Cas_Obj := True;

   case Cas_Obj is
      when (Enum'Val (3) = Enum'(Black)) =>
         Obj1 := 4;
      when others =>
         Failed ("INCORRECT VALUE RETURNED FOR ATTRIBUTE 3");
   end case;
   Cas_Obj := True;

   case Cas_Obj is
      when (True or Flt'Machine_Overflows) =>
         Obj1 := 5;
      when others =>
         Failed ("INCORRECT VALUE RETURNED FOR ATTRIBUTE 4");
   end case;
   Cas_Obj := False;

   case Cas_Obj is
      when (False and Fix'Machine_Rounds) =>
         Obj1 := 6;
      when others =>
         Failed ("INCORRECT VALUE RETURNED FOR ATTRIBUTE 5");
   end case;

   Result;

end C49025a;

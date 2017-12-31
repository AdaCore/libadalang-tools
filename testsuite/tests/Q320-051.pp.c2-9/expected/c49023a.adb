-- C49023A.ADA

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
-- CHECK THAT A CONSTANT DECLARED BY AN OBJECT DECLARATION CAN BE USED IN A
-- STATIC EXPRESSION IF THE CONSTANT WAS DECLARED WITH A STATIC SUBTYPE AND
-- INITIALIZED WITH A STATIC EXPRESSION.

-- L.BROWN  10/01/86

with Report; use Report;
procedure C49023a is

begin
   Test
     ("C49023A",
      "A CONSTANT DECLARED BY AN OBJECT DECLARATION " &
      "UNDER CERTAIN CONDITIONS CAN BE USED IN A " & "STATIC EXPRESSION");
   declare
      type Enum is (Red, Green, Blue, Yellow);
      subtype Senum is Enum range Red .. Blue;
      Conen : constant Senum := Green;
      type Int is range 1 .. 10;
      subtype Sint is Int range 1 .. 5;
      Conin : constant Sint := 3;
      type Flt is digits 3 range 0.0 .. 25.0;
      subtype Sflt is Flt range 10.0 .. 20.0;
      Confl : constant Sflt := 11.0;
      type Fix is delta 0.25 range 0.0 .. 25.0;
      subtype Sfix is Fix range 0.0 .. 12.0;
      Confi  : constant Sfix := 0.25;
      Cas_En : Enum          := Conen;
      type Iteg is range 1 .. Conin;
      type Flty is digits Conin;
      type Fixy is delta Confi range 0.0 .. 10.0;
      type Real is delta 0.25 range 0.0 .. 11.0;
      type Fixty is delta 0.25 range 0.0 .. Confl;

      function Ident_Real (X : Real) return Real;

      package P is
         type T is private;
         Con1 : constant T;
      private
         type T is new Integer;
         Con1 : constant T := 10;
         type Nint is range 1 .. Con1;
      end P;
      package body P is
         type Con2 is range Con1 .. 50;
      begin
         if Nint'Last /= Nint (Ident_Int (10)) then
            Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 1");
         end if;
         if Con2'First /= Con2 (Ident_Int (10)) then
            Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 2");
         end if;
      end P;

      function Ident_Real (X : Real) return Real is
      begin
         if Equal (3, 3) then
            return X;
         else
            return 0.0;
         end if;
      end Ident_Real;

   begin

      if Iteg'Last /= Iteg (Ident_Int (3)) then
         Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 3");
      end if;

      if Flty'Digits /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 4");
      end if;

      if Fixy'Delta /= Ident_Real (0.25) then
         Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 5");
      end if;

      if Fixty'Last /= Fixty (Ident_Real (11.0)) then
         Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 6");
      end if;

      case Cas_En is
         when Conen =>
            Cas_En := Red;
         when others =>
            Failed ("INCORRECT VALUE FOR STATIC EXPRESSION 7");
      end case;

   end;

   Result;

end C49023a;

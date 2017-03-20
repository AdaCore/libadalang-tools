-- C35102A.ADA

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
-- CHECK THAT AN ENUMERATION LITERAL BELONGING TO ONE ENUMERATION TYPE
-- MAY BE DECLARED IN ANOTHER ENUMERATION TYPE DEFINITION IN THE SAME
-- DECLARATIVE REGION.

-- R.WILLIAMS 8/20/86
-- GMT 6/30/87           MOVED THE CALL TO  REPORT.TEST INTO A NEWLY
--                       CREATED PACKAGE NAMED SHOW_TEST_HEADER.
--                       ADDED CODE FOR MY_PACK AND MY_FTN.

with Report; use Report;
procedure C35102a is

   type E1 is ('A', 'B', 'C', Red, Yellow, Blue);
   type E2 is ('A', 'C', Red, Blue);

   package Show_Test_Header is
   -- PURPOSE OF THIS PACKAGE:
   -- WE WANT THE TEST HEADER INFORMATION TO BE
   -- PRINTED  BEFORE  ANY OF THE  PASS/FAIL  MESSAGES.
   end Show_Test_Header;

   package body Show_Test_Header is
   begin
      Test
        ("C35102A",
         "CHECK THAT AN ENUMERATION LITERAL BELONGING " &
         "TO ONE ENUMERATION TYPE MAY BE DECLARED IN " &
         "ANOTHER ENUMERATION TYPE DEFINITION IN THE " &
         "SAME DECLARATIVE REGION");
   end Show_Test_Header;

   function My_Ftn (E : E1) return E2 is
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);
   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN MY_FTN - 1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN MY_FTN - 1");
      end if;

      return E2'Val (Ident_Int (E1'Pos (E)));
   end My_Ftn;

   package My_Pack is
   end My_Pack;

   package body My_Pack is
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);
   begin  -- MY_PACK
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN MY_PACK - 1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN MY_PACK - 1");
      end if;
   end My_Pack;

   package Pkg is
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);

   end Pkg;

   package body Pkg is
   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN PKG - 1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN PKG - 1");
      end if;
   end Pkg;

   package Priv is
      type Enum1 is private;
      type Enum2 is private;

      function Fe1 (E : E1) return Enum1;

      function Fe2 (E : E2) return Enum2;

   private
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);

   end Priv;

   package body Priv is
      function Fe1 (E : E1) return Enum1 is
      begin
         return Enum1'Val (Ident_Int (E1'Pos (E)));
      end Fe1;

      function Fe2 (E : E2) return Enum2 is
      begin
         return Enum2'Val (Ident_Int (E2'Pos (E)));
      end Fe2;

   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN PRIV - 1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN PRIV - 1");
      end if;
   end Priv;

   package Lpriv is
      type Enum1 is limited private;
      type Enum2 is limited private;

      function Fe1 (E : E1) return Enum1;

      function Fe2 (E : E2) return Enum2;

      function Equals (A, B : Enum1) return Boolean;

      function Equals (A, B : Enum2) return Boolean;

   private
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);

   end Lpriv;

   package body Lpriv is
      function Fe1 (E : E1) return Enum1 is
      begin
         return Enum1'Val (Ident_Int (E1'Pos (E)));
      end Fe1;

      function Fe2 (E : E2) return Enum2 is
      begin
         return Enum2'Val (Ident_Int (E2'Pos (E)));
      end Fe2;

      function Equals (A, B : Enum1) return Boolean is
      begin
         if A = B then
            return True;
         else
            return False;
         end if;
      end Equals;

      function Equals (A, B : Enum2) return Boolean is
      begin
         if A = B then
            return True;
         else
            return False;
         end if;
      end Equals;
   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN LPRIV - 1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN LPRIV - 2");
      end if;
   end Lpriv;

   task T1;

   task body T1 is
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);

   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN T1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN T1");
      end if;
   end T1;

   task T2 is
      entry E;
   end T2;

   task body T2 is
   begin
      accept E do
         declare
            type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
            type Enum2 is ('A', 'C', Red, Blue);

         begin
            if Enum2'Succ ('A') /= 'C' then
               Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN T2.E");
            end if;

            if Enum1'Pos (Red) /= 3 then
               Failed ("RED NOT DECLARED CORRECTLY IN " & "ENUM1 IN T2.E");
            end if;
         end;
      end E;
   end T2;

   generic
   procedure Gp1;

   procedure Gp1 is
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);

   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN GP1");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN GP1");
      end if;
   end Gp1;

   generic
      type E1 is (<>);
      type E2 is (<>);
   procedure Gp2;

   procedure Gp2 is
   begin
      if E2'Succ (E2'Value ("'A'")) /= E2'Value ("'C'") then
         Failed ("'A' NOT DECLARED CORRECTLY IN E2 " & "IN GP2");
      end if;

      if E1'Pos (E1'Value ("RED")) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN E1 " & "IN GP2");
      end if;
   end Gp2;

   procedure Newgp1 is new Gp1;
   procedure Newgp2 is new Gp2 (E1, E2);

begin

   declare
      type Enum1 is ('A', 'B', 'C', Red, Yellow, Blue);
      type Enum2 is ('A', 'C', Red, Blue);

   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN BLOCK");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN BLOCK");
      end if;
   end;

   declare
      use Pkg;
   begin
      if Enum2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN PKG - 2");
      end if;

      if Enum1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN PKG - 2");
      end if;
   end;

   declare
      use Priv;
   begin
      if Fe2 (E2'Succ ('A')) /= Fe2 ('C') then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN PRIV - 2");
      end if;

      if Fe1 (Red) /= Fe1 (E1'Val (3)) then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN PRIV - 2");
      end if;
   end;

   declare
      use Lpriv;
   begin
      if not Equals (Fe2 (E2'Succ ('A')), Fe2 ('C')) then
         Failed ("'A' NOT DECLARED CORRECTLY IN ENUM2 " & "IN LPRIV - 2");
      end if;

      if not Equals (Fe1 (Red), Fe1 (E1'Val (3))) then
         Failed ("RED NOT DECLARED CORRECTLY IN ENUM1 " & "IN LPRIV - 2");
      end if;
   end;

   begin
      if E2'Succ ('A') /= 'C' then
         Failed ("'A' NOT DECLARED CORRECTLY IN E2");
      end if;

      if E1'Pos (Red) /= 3 then
         Failed ("RED NOT DECLARED CORRECTLY IN E1");
      end if;
   end;

   Newgp1;
   Newgp2;
   T2.E;

   Result;
end C35102a;

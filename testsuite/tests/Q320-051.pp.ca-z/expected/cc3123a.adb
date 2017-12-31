-- CC3123A.ADA

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
-- CHECK THAT DEFAULT EXPRESSIONS FOR GENERIC IN PARAMETERS ARE ONLY EVALUATED
-- IF THERE ARE NO ACTUAL PARAMETERS.

-- TBN  12/01/86

with Report; use Report;
procedure Cc3123a is

begin
   Test
     ("CC3123A",
      "CHECK THAT DEFAULT EXPRESSIONS FOR GENERIC IN " &
      "PARAMETERS ARE ONLY EVALUATED IF THERE ARE " & "NO ACTUAL PARAMETERS");
   declare
      type Enum is (I, Ii, Iii);
      Obj_Int  : Integer := 1;
      Obj_Enum : Enum    := I;

      generic
         Gen_Int : in Integer := Ident_Int (2);
         Gen_Bool : in Boolean := Ident_Bool (False);
         Gen_Enum : in Enum := Ii;
      package P is
         Pac_Int  : Integer := Gen_Int;
         Pac_Bool : Boolean := Gen_Bool;
         Pac_Enum : Enum    := Gen_Enum;
      end P;

      package P1 is new P;
      package P2 is new P (Ident_Int (Obj_Int), Gen_Enum => Obj_Enum);
      package P3 is new P (Gen_Bool => Ident_Bool (True));
   begin
      if P1.Pac_Int /= 2 or P1.Pac_Bool or P1.Pac_Enum /= Ii then
         Failed ("DEFAULT VALUES WERE NOT EVALUATED");
      end if;
      if P2.Pac_Int /= 1 or P2.Pac_Bool or P2.Pac_Enum /= I then
         Failed ("DEFAULT VALUES WERE NOT EVALUATED CORRECTLY " & "- 1");
      end if;
      if P3.Pac_Int /= 2 or not (P3.Pac_Bool) or P3.Pac_Enum /= Ii then
         Failed ("DEFAULT VALUES WERE NOT EVALUATED CORRECTLY " & "- 2");
      end if;
   end;

   -------------------------------------------------------------------
   declare
      Obj_Int1 : Integer := 3;

      function Func (X : Integer) return Integer;

      generic
         Gen_Int1 : in Integer := Func (1);
         Gen_Int2 : in Integer := Func (Gen_Int1 + 1);
      procedure Proc;

      procedure Proc is
         Proc_Int1 : Integer := Gen_Int1;
         Proc_Int2 : Integer := Gen_Int2;
      begin
         if Proc_Int1 /= 3 then
            Failed ("DEFAULT VALUES WERE NOT EVALUATED " & "CORRECTLY - 3");
         end if;
         if Proc_Int2 /= 4 then
            Failed ("DEFAULT VALUES WERE NOT EVALUATED " & "CORRECTLY - 4");
         end if;
      end Proc;

      function Func (X : Integer) return Integer is
      begin
         if X /= Ident_Int (4) then
            Failed ("DEFAULT VALUES WERE NOT EVALUATED " & "CORRECTLY - 5");
         end if;
         return Ident_Int (X);
      end Func;

      procedure New_Proc is new Proc (Gen_Int1 => Obj_Int1);

   begin
      New_Proc;
   end;

   -------------------------------------------------------------------
   declare
      type Ara_Typ is array (1 .. 2) of Integer;
      type Rec is record
         Ans : Boolean;
         Ara : Ara_Typ;
      end record;
      type Ara_Rec is array (1 .. 5) of Rec;

      function F (X : Integer) return Integer;

      Obj_Rec : Rec     := (False, (3, 4));
      Obj_Ara : Ara_Rec := (1 .. 5 => (False, (3, 4)));

      generic
         Gen_Obj1 : in Ara_Typ := (F (1), 2);
         Gen_Obj2 : in Rec := (True, Gen_Obj1);
         Gen_Obj3 : in Ara_Rec := (1 .. F (5) => (True, (1, 2)));
      function Func return Integer;

      function Func return Integer is
      begin
         return Ident_Int (1);
      end Func;

      function F (X : Integer) return Integer is
      begin
         Failed ("DEFAULT VALUES WERE EVALUATED - 1");
         return Ident_Int (X);
      end F;

      function New_Func is new Func ((3, 4), Obj_Rec, Obj_Ara);

   begin
      if not Equal (New_Func, 1) then
         Failed ("INCORRECT RESULT FROM GENERIC FUNCTION - 1");
      end if;
   end;

   -------------------------------------------------------------------
   declare
      subtype Int is Integer range 1 .. 5;
      type Ara_Typ is array (1 .. 2) of Integer;
      type Color is (Red, White);
      type Con_Rec (D : Int) is record
         A : Color;
         B : Ara_Typ;
      end record;
      type Uncon_Or_Con_Rec (D : Int := 2) is record
         A : Color;
         B : Ara_Typ;
      end record;
      function F (X : Color) return Color;

      Obj_Con1  : Con_Rec (1)          := (1, White, (3, 4));
      Obj_Uncon : Uncon_Or_Con_Rec     := (2, White, (3, 4));
      Obj_Con2  : Uncon_Or_Con_Rec (3) := (3, White, (3, 4));

      generic
         Gen_Con1 : in Con_Rec := (2, F (Red), (1, 2));
         Gen_Uncon : in Uncon_Or_Con_Rec := (2, F (Red), (1, 2));
         Gen_Con2 : in Uncon_Or_Con_Rec := Gen_Uncon;
      function Func return Integer;

      function Func return Integer is
      begin
         return Ident_Int (1);
      end Func;

      function F (X : Color) return Color is
      begin
         Failed ("DEFAULT VALUES WERE EVALUATED - 2");
         return White;
      end F;

      function New_Func is new Func (Obj_Con1, Obj_Uncon, Obj_Con2);

   begin
      if not Equal (New_Func, 1) then
         Failed ("INCORRECT RESULT FROM GENERIC FUNCTION - 2");
      end if;
   end;

   Result;
end Cc3123a;

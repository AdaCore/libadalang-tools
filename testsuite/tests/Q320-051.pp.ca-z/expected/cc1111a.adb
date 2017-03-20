-- CC1111A.ADA

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
--     CHECK THAT AFTER A GENERIC UNIT IS INSTANTIATED, THE SUBTYPE OF
--     AN IN OUT OBJECT PARAMETER IS DETERMINED BY THE ACTUAL PARAMETER
--     (TESTS INTEGER, ENUMERATION, FLOATING POINT, FIXED POINT, ARRAY,
--     ACCESS, AND DISCRIMINATED TYPES).

-- HISTORY:
--     BCB 03/28/88  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure Cc1111a is

   subtype Int is Integer range 0 .. 5;
   Intvar : Integer range 1 .. 3;

   type Enum is (One, Two, Three, Four, Five, Six, Seven, Eight);
   subtype Subenum is Enum range One .. Five;
   Enumvar : Enum range Two .. Three;

   type Flt is digits 5 range -5.0 .. 5.0;
   subtype Subflt is Flt range -1.0 .. 1.0;
   Fltvar : Flt range 0.0 .. 1.0;

   type Fix is delta 0.5 range -5.0 .. 5.0;
   subtype Subfix is Fix range -1.0 .. 1.0;
   Fixvar : Fix range 0.0 .. 1.0;

   subtype Str is String (1 .. 10);
   Strvar : String (1 .. 5);

   type Rec (Disc : Integer := 5) is record
      null;
   end record;
   subtype Subrec is Rec (6);
   Recvar    : Rec (5);
   Subrecvar : Subrec;

   type Accrec is access Rec;
   subtype A1 is Accrec (1);
   subtype A2 is Accrec (2);
   A1var : A1 := new Rec (1);
   A2var : A2 := new Rec (2);

   package P is
      type Priv is private;
   private
      type Priv is range 1 .. 100;
      subtype Subpriv is Priv range 5 .. 10;
      Privvar : Priv range 8 .. 10;
   end P;

   package body P is
      function Privequal (One, Two : Subpriv) return Boolean;

      function Privequal (One, Two : Subpriv) return Boolean is
      begin
         return One = Two;
      end Privequal;

      generic
         Input : Subpriv;
         Output : in out Subpriv;
      procedure I;

      procedure I is
      begin
         Output := Input;
         Failed
           ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " & "PRIVATE TYPE");
         if Privequal (Output, Output) then
            Comment ("DON'T OPTIMIZE OUTPUT");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED");
      end I;

      procedure I1 is new I (5, Privvar);
      procedure I2 is new I (Subpriv'First, Privvar);

   begin
      Test
        ("CC1111A",
         "CHECK THAT AFTER A GENERIC UNIT IS " &
         "INSTANTIATED, THE SUBTYPE OF AN IN OUT " &
         "OBJECT PARAMETER IS DETERMINED BY THE " &
         "ACTUAL PARAMETER (TESTS INTEGER, " &
         "ENUMERATION, FLOATING POINT, FIXED POINT " &
         ", ARRAY, ACCESS, AND DISCRIMINATED TYPES)");

      I1;
      I2;
   end P;

   use P;

   generic
      type Gp is private;
   function Gen_Ident (X : Gp) return Gp;

   generic
      Input : Int;
      Output : in out Int;
   procedure B;

   generic
      Input : Subenum;
      Output : in out Subenum;
   procedure C;

   generic
      Input : Subflt;
      Output : in out Subflt;
   procedure D;

   generic
      Input : Subfix;
      Output : in out Subfix;
   procedure E;

   generic
      Input : Str;
      Output : in out Str;
   procedure F;

   generic
      Input : A1;
      Output : in out A1;
   procedure G;

   generic
      Input : Subrec;
      Output : in out Subrec;
   procedure H;

   generic
      type Gp is private;
   function Genequal (One, Two : Gp) return Boolean;

   function Genequal (One, Two : Gp) return Boolean is
   begin
      return One = Two;
   end Genequal;

   function Gen_Ident (X : Gp) return Gp is
   begin
      return X;
   end Gen_Ident;

   function Int_Ident is new Gen_Ident (Int);
   function Subenum_Ident is new Gen_Ident (Subenum);
   function Subflt_Ident is new Gen_Ident (Subflt);
   function Subfix_Ident is new Gen_Ident (Subfix);

   function Enumequal is new Genequal (Subenum);
   function Fltequal is new Genequal (Subflt);
   function Fixequal is new Genequal (Subfix);
   function Strequal is new Genequal (Str);
   function Accequal is new Genequal (A2);
   function Recequal is new Genequal (Rec);

   procedure B is
   begin
      Output := Input;
      Failed
        ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " & "INTEGER TYPE");
      if Equal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end B;

   procedure C is
   begin
      Output := Input;
      Failed
        ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " & "ENUMERATION TYPE");
      if Enumequal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end C;

   procedure D is
   begin
      Output := Input;
      Failed
        ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " &
         "FLOATING POINT TYPE");
      if Fltequal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end D;

   procedure E is
   begin
      Output := Input;
      Failed
        ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " & "FIXED POINT TYPE");
      if Fixequal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end E;

   procedure F is
   begin
      Output := Input;
      Failed ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " & "ARRAY TYPE");
      if Strequal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end F;

   procedure G is
   begin
      Output := Input;
      Failed ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " & "ACCESS TYPE");
      if Accequal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end G;

   procedure H is
   begin
      Output := Input;
      Failed
        ("SUBTYPE NOT DETERMINED BY ACTUAL PARAMETER - " &
         "DISCRIMINATED RECORD TYPE");
      if Recequal (Output, Output) then
         Comment ("DON'T OPTIMIZE OUTPUT");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end H;

   procedure B1 is new B (4, Intvar);
   procedure C1 is new C (Four, Enumvar);
   procedure D1 is new D (-1.0, Fltvar);
   procedure E1 is new E (-1.0, Fixvar);
   procedure F1 is new F ("9876543210", Strvar);
   procedure G1 is new G (A1var, A2var);
   procedure H1 is new H (Subrecvar, Recvar);

   procedure B2 is new B (Int_Ident (Int'First), Intvar);
   procedure C2 is new C (Subenum_Ident (Subenum'First), Enumvar);
   procedure D2 is new D (Subflt_Ident (Subflt'First), Fltvar);
   procedure E2 is new E (Subfix_Ident (Subfix'First), Fixvar);

begin

   B1;
   C1;
   D1;
   E1;
   F1;
   G1;
   H1;

   B2;
   C2;
   D2;
   E2;

   Result;
end Cc1111a;

-- C74203A.ADA

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
--     CHECK THAT MEMBERSHIP TESTS, QUALIFICATION, AND EXPLICIT
--     CONVERSION ARE AVAILABLE FOR LIMITED AND NON-LIMITED PRIVATE
--     TYPES.  INCLUDE TYPES WITH DISCRIMINANTS AND TYPES
--     WITH LIMITED COMPONENTS.

-- HISTORY:
--     BCB 03/10/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C74203a is

   package Pp is
      type Lim is limited private;
      procedure Init (Z1 : out Lim; Z2 : Integer);

      type A is private;
      subtype Suba is A;
      A1 : constant A;

      type B is limited private;
      B1 : constant B;

      type C is private;
      C1 : constant C;

      type D is limited private;
      D1 : constant D;

      type E (Disc1 : Integer := 5) is private;
      subtype Sube is E;
      E1 : constant E;

      type F (Disc2 : Integer := 15) is limited private;
      F1 : constant F;

      type G (Disc3 : Integer) is private;
      G1 : constant G;

      type H (Disc4 : Integer) is limited private;
      H1 : constant H;

      type I is record
         Compi : Lim;
      end record;
      subtype Subi is I;

      type J is array (1 .. 5) of Lim;
      subtype Subj is J;

      type S1 is (Vince, Tom, Phil, Jodie, Rosa, Teresa);
      type S2 is (This, That, These, Those, Them);
      type S3 is range 1 .. 100;
      type S4 is range 1 .. 100;
   private
      type Lim is range 1 .. 100;

      type A is (Red, Blue, Green, Yellow, Black, White);
      A1 : constant A := Blue;

      type B is (One, Two, Three, Four, Five, Six);
      B1 : constant B := Three;

      type C is range 1 .. 100;
      C1 : constant C := 50;

      type D is range 1 .. 100;
      D1 : constant D := 50;

      type E (Disc1 : Integer := 5) is record
         Compe : S1;
      end record;
      E1 : constant E := (Disc1 => 5, Compe => Tom);

      type F (Disc2 : Integer := 15) is record
         Compf : S2;
      end record;
      F1 : constant F := (Disc2 => 15, Compf => That);

      type G (Disc3 : Integer) is record
         Compg : S3;
      end record;
      G1 : constant G := (Disc3 => 25, Compg => 50);

      type H (Disc4 : Integer) is record
         Comph : S4;
      end record;
      H1 : constant H := (Disc4 => 30, Comph => 50);
   end Pp;

   use Pp;

   Avar : Suba := A1;
   Evar : Sube := E1;

   Ivar : Subi;
   Jvar : Subj;

   package body Pp is
      procedure Init (Z1 : out Lim; Z2 : Integer) is
      begin
         Z1 := Lim (Z2);
      end Init;
   begin
      null;
   end Pp;

   procedure Qual_Priv (W : A) is
   begin
      null;
   end Qual_Priv;

   procedure Qual_Lim_Priv (X : B) is
   begin
      null;
   end Qual_Lim_Priv;

   procedure Expl_Conv_Priv_1 (Y : C) is
   begin
      null;
   end Expl_Conv_Priv_1;

   procedure Expl_Conv_Lim_Priv_1 (Z : D) is
   begin
      null;
   end Expl_Conv_Lim_Priv_1;

   procedure Expl_Conv_Priv_2 (Y2 : G) is
   begin
      null;
   end Expl_Conv_Priv_2;

   procedure Expl_Conv_Lim_Priv_2 (Z2 : H) is
   begin
      null;
   end Expl_Conv_Lim_Priv_2;

   procedure Expl_Conv_Priv_3 (Y3 : I) is
   begin
      null;
   end Expl_Conv_Priv_3;

   procedure Expl_Conv_Priv_4 (Y4 : J) is
   begin
      null;
   end Expl_Conv_Priv_4;

begin
   Test
     ("C74203A",
      "CHECK THAT MEMBERSHIP TESTS, QUALIFICATION, " &
      "AND EXPLICIT CONVERSION ARE AVAILABLE FOR " &
      "LIMITED AND NON-LIMITED PRIVATE TYPES.  " &
      "INCLUDE TYPES WITH DISCRIMINANTS AND " &
      "TYPES WITH LIMITED COMPONENTS");

   Init (Ivar.Compi, 50);

   for K in Ident_Int (1) .. Ident_Int (5) loop
      Init (Jvar (K), 25);
   end loop;

   if not (Avar in A) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
         "PRIVATE TYPE - 1");
   end if;

   if (Avar not in A) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
         "PRIVATE TYPE - 1");
   end if;

   if not (B1 in B) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
         "LIMITED PRIVATE TYPE - 1");
   end if;

   if (B1 not in B) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
         "LIMITED PRIVATE TYPE - 1");
   end if;

   Qual_Priv (A'(Avar));

   Qual_Lim_Priv (B'(B1));

   Expl_Conv_Priv_1 (C (C1));

   Expl_Conv_Lim_Priv_1 (D (D1));

   if not (Evar in E) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
         "PRIVATE TYPE - 2");
   end if;

   if (Evar not in E) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
         "PRIVATE TYPE - 2");
   end if;

   if not (F1 in F) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
         "LIMITED PRIVATE TYPE - 2");
   end if;

   if (F1 not in F) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
         "LIMITED PRIVATE TYPE - 2");
   end if;

   Expl_Conv_Priv_2 (G (G1));

   Expl_Conv_Lim_Priv_2 (H (H1));

   if not (Ivar in I) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
         "PRIVATE TYPE - 3");
   end if;

   if (Ivar not in I) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
         "PRIVATE TYPE - 3");
   end if;

   Expl_Conv_Priv_3 (I (Ivar));

   if not (Jvar in J) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
         "PRIVATE TYPE - 4");
   end if;

   if (Jvar not in J) then
      Failed
        ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
         "PRIVATE TYPE - 4");
   end if;

   Expl_Conv_Priv_4 (J (Jvar));

   Result;
end C74203a;

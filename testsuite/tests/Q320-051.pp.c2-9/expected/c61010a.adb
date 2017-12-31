-- C61010A.ADA

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
-- CHECK THAT AN IN OR IN OUT FORMAL PARAMETER CAN BE DECLARED WITH A LIMITED
-- PRIVATE TYPE OR A LIMITED COMPOSITE TYPE.

-- DAS  1/22/81
-- JRK 1/20/84 TOTALLY REVISED.

with Report; use Report;
procedure C61010a is

   package Pkg is

      type Itype is limited private;

      procedure Look_In_I (X : in Itype; V : Integer; M : String);

      procedure Look_Inout_I (X : in out Itype; V : Integer; M : String);

      procedure Set_I (X : in out Itype; V : Integer);

      subtype Int_0_20 is Integer range 0 .. 20;
      type Vrtype (C : Int_0_20 := 20) is limited private;

      procedure Look_In_Vr (X : in Vrtype; C : Integer; I : Integer;
         S                    :    String; M : String);

      procedure Look_Inout_Vr (X : in out Vrtype; C : Integer; I : Integer;
         S                       :        String; M : String);

      procedure Set_Vr (X : in out Vrtype; C : Integer; I : Integer;
         S                :        String);

   private

      type Itype is new Integer range 0 .. 99;

      type Vrtype (C : Int_0_20 := 20) is record
         I : Integer;
         S : String (1 .. C);
      end record;

   end Pkg;

   use Pkg;

   I1 : Itype;

   type Atype is array (1 .. 3) of Itype;

   A1 : Atype;

   Vr1 : Vrtype;

   D : constant Int_0_20 := 10;

   type Rtype is record
      J : Itype;
      R : Vrtype (D);
   end record;

   R1 : Rtype;

   package body Pkg is

      procedure Look_In_I (X : in Itype; V : Integer; M : String) is
      begin
         if Integer (X) /= V then
            Failed ("WRONG SCALAR VALUE - " & M);
         end if;
      end Look_In_I;

      procedure Look_Inout_I (X : in out Itype; V : Integer; M : String) is
      begin
         if Integer (X) /= V then
            Failed ("WRONG SCALAR VALUE - " & M);
         end if;
      end Look_Inout_I;

      procedure Set_I (X : in out Itype; V : Integer) is
      begin
         X := Itype (Ident_Int (V));
      end Set_I;

      procedure Look_In_Vr (X : in Vrtype; C : Integer; I : Integer;
         S                    :    String; M : String)
      is
      begin
         if (X.C /= C or X.I /= I) or else X.S /= S then
            Failed ("WRONG COMPOSITE VALUE - " & M);
         end if;
      end Look_In_Vr;

      procedure Look_Inout_Vr (X : in out Vrtype; C : Integer; I : Integer;
         S                       :        String; M : String)
      is
      begin
         if (X.C /= C or X.I /= I) or else X.S /= S then
            Failed ("WRONG COMPOSITE VALUE - " & M);
         end if;
      end Look_Inout_Vr;

      procedure Set_Vr (X : in out Vrtype; C : Integer; I : Integer;
         S                :        String)
      is
      begin
         X := (Ident_Int (C), Ident_Int (I), Ident_Str (S));
      end Set_Vr;

   begin
      I1 := Itype (Ident_Int (2));

      for I in A1'Range loop
         A1 (I) := Itype (3 + Ident_Int (I));
      end loop;

      Vr1 := (Ident_Int (5), Ident_Int (4), Ident_Str ("01234"));

      R1.J := Itype (Ident_Int (6));
      R1.R := (Ident_Int (D), Ident_Int (19), Ident_Str ("ABCDEFGHIJ"));
   end Pkg;

   procedure Check_In_I (X : in Itype; V : Integer; M : String) is
   begin
      Look_In_I (X, V, M);
   end Check_In_I;

   procedure Check_Inout_I (X : in out Itype; Ov : Integer; Nv : Integer;
      M                       :        String)
   is
   begin
      Look_Inout_I (X, Ov, M & " - A");
      Set_I (X, Nv);
      Look_Inout_I (X, Nv, M & " - B");
      Look_In_I (X, Nv, M & " - C");
   end Check_Inout_I;

   procedure Check_In_A (X : in Atype; V : Integer; M : String) is
   begin
      for I in X'Range loop
         Look_In_I (X (I), V + I, M & " -" & Integer'Image (I));
      end loop;
   end Check_In_A;

   procedure Check_Inout_A (X : in out Atype; Ov : Integer; Nv : Integer;
      M                       :        String)
   is
   begin
      for I in X'Range loop
         Look_Inout_I (X (I), Ov + I, M & " - A" & Integer'Image (I));
         Set_I (X (I), Nv + I);
         Look_Inout_I (X (I), Nv + I, M & " - B" & Integer'Image (I));
         Look_In_I (X (I), Nv + I, M & " - C" & Integer'Image (I));
      end loop;
   end Check_Inout_A;

   procedure Check_In_Vr (X : in Vrtype; C : Integer; I : Integer; S : String;
      M                     :    String)
   is
   begin
      Look_In_Vr (X, C, I, S, M);
   end Check_In_Vr;

   procedure Check_Inout_Vr (X : in out Vrtype; Oc : Integer; Oi : Integer;
      Os :        String; Nc : Integer; Ni : Integer; Ns : String; M : String)
   is
   begin
      Look_Inout_Vr (X, Oc, Oi, Os, M & " - A");
      Set_Vr (X, Nc, Ni, Ns);
      Look_Inout_Vr (X, Nc, Ni, Ns, M & " - B");
      Look_In_Vr (X, Nc, Ni, Ns, M & " - C");
   end Check_Inout_Vr;

   procedure Check_In_R (X : in Rtype; J : Integer; C : Integer; I : Integer;
      S                    :    String; M : String)
   is
   begin
      Look_In_I (X.J, J, M & " - A");
      Look_In_Vr (X.R, C, I, S, M & " - B");
   end Check_In_R;

   procedure Check_Inout_R (X : in out Rtype; Oj : Integer; Oc : Integer;
      Oi :    Integer; Os : String; Nj : Integer; Nc : Integer; Ni : Integer;
      Ns                      :        String; M : String)
   is
   begin
      Look_Inout_I (X.J, Oj, M & " - A");
      Look_Inout_Vr (X.R, Oc, Oi, Os, M & " - B");
      Set_I (X.J, Nj);
      Set_Vr (X.R, Nc, Ni, Ns);
      Look_Inout_I (X.J, Nj, M & " - C");
      Look_Inout_Vr (X.R, Nc, Ni, Ns, M & " - D");
      Look_In_I (X.J, Nj, M & " - E");
      Look_In_Vr (X.R, Nc, Ni, Ns, M & " - F");
   end Check_Inout_R;

begin
   Test
     ("C61010A",
      "CHECK THAT LIMITED PRIVATE/COMPOSITE TYPES " &
      "CAN BE USED AS IN OR IN OUT FORMAL PARAMETERS");

   Check_In_I (I1, 2, "IN I");

   Check_Inout_I (I1, 2, 5, "INOUT I");

   Check_In_A (A1, 3, "IN A");

   Check_Inout_A (A1, 3, 17, "INOUT A");

   Check_In_Vr (Vr1, 5, 4, "01234", "IN VR");

   Check_Inout_Vr (Vr1, 5, 4, "01234", 10, 11, "9876543210", "INOUT VR");

   Check_In_R (R1, 6, D, 19, "ABCDEFGHIJ", "IN R");

   Check_Inout_R
     (R1, 6, D, 19, "ABCDEFGHIJ", 13, D, 5, "ZYXWVUTSRQ", "INOUT R");

   Result;
end C61010a;

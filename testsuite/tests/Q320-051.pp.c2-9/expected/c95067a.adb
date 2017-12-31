-- C95067A.ADA

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
-- CHECK THAT A FORMAL PARAMETER OF MODE IN OR IN OUT CAN BE OF A LIMITED TYPE,
-- INCLUDING A COMPOSITE LIMITED TYPE.

-- JWC 6/20/85

with Report; use Report;
procedure C95067a is

   package Pkg is

      type Itype is limited private;

      task T1 is

         entry Look_In_I (X : in Itype; V : Integer; M : String);

         entry Look_Inout_I (X : in out Itype; V : Integer; M : String);

         entry Set_I (X : in out Itype; V : Integer);

      end T1;

      subtype Int_0_20 is Integer range 0 .. 20;
      type Vrtype (C : Int_0_20 := 20) is limited private;

      task T2 is

         entry Look_In_Vr (X : in Vrtype; C : Integer; I : Integer; S : String;
            M                :    String);

         entry Look_Inout_Vr (X : in out Vrtype; C : Integer; I : Integer;
            S                   :        String; M : String);

         entry Set_Vr (X : in out Vrtype; C : Integer; I : Integer;
            S            :        String);

      end T2;

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

      task body T1 is
      begin
         loop
            select
               accept Look_In_I (X : in Itype; V : Integer; M : String) do
                  if Integer (X) /= V then
                     Failed ("WRONG SCALAR VALUE - " & M);
                  end if;
               end Look_In_I;
            or
               accept Look_Inout_I (X : in out Itype; V : Integer; M : String)
               do
                  if Integer (X) /= V then
                     Failed ("WRONG SCALAR VALUE - " & M);
                  end if;
               end Look_Inout_I;
            or
               accept Set_I (X : in out Itype; V : Integer) do
                  X := Itype (Ident_Int (V));
               end Set_I;
            or
               terminate;
            end select;
         end loop;
      end T1;

      task body T2 is
      begin
         loop
            select
               accept Look_In_Vr (X : in Vrtype; C : Integer; I : Integer;
                  S                 :    String; M : String) do
                  if (X.C /= C or X.I /= I) or else X.S /= S then
                     Failed ("WRONG COMPOSITE VALUE - " & M);
                  end if;
               end Look_In_Vr;
            or
               accept Look_Inout_Vr (X : in out Vrtype; C : Integer;
                  I                    :    Integer; S : String; M : String) do
                  if (X.C /= C or X.I /= I) or else X.S /= S then
                     Failed ("WRONG COMPOSITE VALUE - " & M);
                  end if;
               end Look_Inout_Vr;
            or
               accept Set_Vr (X : in out Vrtype; C : Integer; I : Integer;
                  S             :        String) do
                  X := (Ident_Int (C), Ident_Int (I), Ident_Str (S));
               end Set_Vr;
            or
               terminate;
            end select;
         end loop;
      end T2;

   begin
      I1 := Itype (Ident_Int (2));

      for I in A1'Range loop
         A1 (I) := Itype (3 + Ident_Int (I));
      end loop;

      Vr1 := (Ident_Int (5), Ident_Int (4), Ident_Str ("01234"));

      R1.J := Itype (Ident_Int (6));
      R1.R := (Ident_Int (D), Ident_Int (19), Ident_Str ("ABCDEFGHIJ"));
   end Pkg;

   task T3 is
      entry Check_In_I (X : in Itype; V : Integer; M : String);

      entry Check_Inout_I (X : in out Itype; Ov : Integer; Nv : Integer;
         M                   :        String);

      entry Check_In_A (X : in Atype; V : Integer; M : String);

      entry Check_Inout_A (X : in out Atype; Ov : Integer; Nv : Integer;
         M                   :        String);

      entry Check_In_Vr (X : in Vrtype; C : Integer; I : Integer; S : String;
         M                 :    String);

      entry Check_Inout_Vr (X : in out Vrtype; Oc : Integer; Oi : Integer;
         Os :    String; Nc : Integer; Ni : Integer; Ns : String; M : String);

      entry Check_In_R (X : in Rtype; J : Integer; C : Integer; I : Integer;
         S                :    String; M : String);

      entry Check_Inout_R (X : in out Rtype; Oj : Integer; Oc : Integer;
         Oi : Integer; Os : String; Nj : Integer; Nc : Integer; Ni : Integer;
         Ns                  :        String; M : String);
   end T3;

   task body T3 is
   begin
      accept Check_In_I (X : in Itype; V : Integer; M : String) do
         T1.Look_In_I (X, V, M);
      end Check_In_I;

      accept Check_Inout_I (X : in out Itype; Ov : Integer; Nv : Integer;
         M                    :        String) do
         T1.Look_Inout_I (X, Ov, M & " - A");
         T1.Set_I (X, Nv);
         T1.Look_Inout_I (X, Nv, M & " - B");
         T1.Look_In_I (X, Nv, M & " - C");
      end Check_Inout_I;

      accept Check_In_A (X : in Atype; V : Integer; M : String) do
         for I in X'Range loop
            T1.Look_In_I (X (I), V + I, M & " -" & Integer'Image (I));
         end loop;
      end Check_In_A;

      accept Check_Inout_A (X : in out Atype; Ov : Integer; Nv : Integer;
         M                    :        String) do
         for I in X'Range loop
            T1.Look_Inout_I (X (I), Ov + I, M & " - A" & Integer'Image (I));
            T1.Set_I (X (I), Nv + I);
            T1.Look_Inout_I (X (I), Nv + I, M & " - B" & Integer'Image (I));
            T1.Look_In_I (X (I), Nv + I, M & " - C" & Integer'Image (I));
         end loop;
      end Check_Inout_A;

      accept Check_In_Vr (X : in Vrtype; C : Integer; I : Integer; S : String;
         M                  :    String) do
         T2.Look_In_Vr (X, C, I, S, M);
      end Check_In_Vr;

      accept Check_Inout_Vr (X : in out Vrtype; Oc : Integer; Oi : Integer;
         Os : String; Nc : Integer; Ni : Integer; Ns : String; M : String) do
         T2.Look_Inout_Vr (X, Oc, Oi, Os, M & " - A");
         T2.Set_Vr (X, Nc, Ni, Ns);
         T2.Look_Inout_Vr (X, Nc, Ni, Ns, M & " - B");
         T2.Look_In_Vr (X, Nc, Ni, Ns, M & " - C");
      end Check_Inout_Vr;

      accept Check_In_R (X : in Rtype; J : Integer; C : Integer; I : Integer;
         S                 :    String; M : String) do
         T1.Look_In_I (X.J, J, M & " - A");
         T2.Look_In_Vr (X.R, C, I, S, M & " - B");
      end Check_In_R;

      accept Check_Inout_R (X : in out Rtype; Oj : Integer; Oc : Integer;
         Oi : Integer; Os : String; Nj : Integer; Nc : Integer; Ni : Integer;
         Ns                   :        String; M : String) do
         T1.Look_Inout_I (X.J, Oj, M & " - A");
         T2.Look_Inout_Vr (X.R, Oc, Oi, Os, M & " - B");
         T1.Set_I (X.J, Nj);
         T2.Set_Vr (X.R, Nc, Ni, Ns);
         T1.Look_Inout_I (X.J, Nj, M & " - C");
         T2.Look_Inout_Vr (X.R, Nc, Ni, Ns, M & " - D");
         T1.Look_In_I (X.J, Nj, M & " - E");
         T2.Look_In_Vr (X.R, Nc, Ni, Ns, M & " - F");
      end Check_Inout_R;
   end T3;

begin
   Test
     ("C95067A",
      "CHECK THAT LIMITED PRIVATE/COMPOSITE TYPES " &
      "CAN BE USED AS IN OR IN OUT FORMAL PARAMETERS");

   T3.Check_In_I (I1, 2, "IN I");

   T3.Check_Inout_I (I1, 2, 5, "INOUT I");

   T3.Check_In_A (A1, 3, "IN A");

   T3.Check_Inout_A (A1, 3, 17, "INOUT A");

   T3.Check_In_Vr (Vr1, 5, 4, "01234", "IN VR");

   T3.Check_Inout_Vr (Vr1, 5, 4, "01234", 10, 11, "9876543210", "INOUT VR");

   T3.Check_In_R (R1, 6, D, 19, "ABCDEFGHIJ", "IN R");

   T3.Check_Inout_R
     (R1, 6, D, 19, "ABCDEFGHIJ", 13, D, 5, "ZYXWVUTSRQ", "INOUT R");

   Result;
end C95067a;

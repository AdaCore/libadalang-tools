--C37404B.ADA

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
--     CHECK THAT 'CONSTRAINED IS FALSE FOR VARIABLES THAT HAVE
--     DISCRIMINANTS WITH DEFAULT VALUES.

-- HISTORY:
--     LDC 06/08/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C37404b is

   subtype Int is Integer range 1 .. 10;

   type Rec_Def (A : Int := 5) is record
      I : Int := 1;
   end record;

   subtype Rec_Def_Sub is Rec_Def;

   type Rec_Def_Arr is array (Integer range -8 .. 7) of Rec_Def;
   type Rec_Def_Sarr is array (Integer range -8 .. 7) of Rec_Def_Sub;

   package Pri_Pack is
      type Rec_Def_Pri (A : Integer := 5) is private;
      type Rec_Def_Lim_Pri (A : Integer := 5) is limited private;

   private

      type Rec_Def_Pri (A : Integer := 5) is record
         I : Integer := 1;
      end record;

      type Rec_Def_Lim_Pri (A : Integer := 5) is record
         I : Integer := 1;
      end record;

   end Pri_Pack;
   use Pri_Pack;

   A : Rec_Def;
   B : Rec_Def_Sub;
   C : array (0 .. 15) of Rec_Def;
   D : array (0 .. 15) of Rec_Def_Sub;
   E : Rec_Def_Arr;
   F : Rec_Def_Sarr;
   G : Rec_Def_Pri;
   H : Rec_Def_Lim_Pri;

   Z : Rec_Def;

   procedure Subprog (Rec : out Rec_Def) is

   begin
      if Rec'Constrained then
         Failed
           ("'CONSTRAINED TRUE FOR SUBPROGRAM OUT " &
            "PARAMETER INSIDE THE SUBPROGRAM");
      end if;
   end Subprog;

begin
   Test
     ("C37404B",
      "CHECK THAT 'CONSTRAINED IS FALSE FOR VARIABLES" &
      " THAT HAVE DISCRIMINANTS WITH DEFAULT VALUES.");

   if A'Constrained then
      Failed ("'CONSTRAINED TRUE FOR RECORD COMPONENT");
   end if;

   if B'Constrained then
      Failed ("'CONSTRAINED TRUE FOR SUBTYPE");
   end if;

   if C (1)'Constrained then
      Failed ("'CONSTRAINED TRUE FOR ARRAY TYPE");
   end if;

   if D (1)'Constrained then
      Failed ("'CONSTRAINED TRUE FOR ARRAY OF SUBTYPE");
   end if;

   if E (1)'Constrained then
      Failed ("'CONSTRAINED TRUE FOR ARRAY TYPE");
   end if;

   if F (1)'Constrained then
      Failed ("'CONSTRAINED TRUE FOR ARRAY OF SUBTYPE");
   end if;

   if G'Constrained then
      Failed ("'CONSTRAINED TRUE FOR PRIVATE TYPE");
   end if;

   if H'Constrained then
      Failed ("'CONSTRAINED TRUE FOR LIMITED PRIVATE TYPE");
   end if;

   Subprog (Z);
   if Z'Constrained then
      Failed
        ("'CONSTRAINED TRUE FOR SUBPROGRAM OUT PARAMETER " & "AFTER THE CALL");
   end if;

   if Ident_Int (A.I) /= 1 or Ident_Int (B.I) /= 1 or
     Ident_Int (C (1).I) /= 1 or Ident_Int (D (1).I) /= 1 or
     Ident_Int (E (1).I) /= 1 or Ident_Int (F (1).I) /= 1 or
     Ident_Int (Z.I) /= 1 or Ident_Int (A.A) /= 5 or Ident_Int (B.A) /= 5 or
     Ident_Int (C (1).A) /= 5 or Ident_Int (D (1).A) /= 5 or
     Ident_Int (E (1).A) /= 5 or Ident_Int (F (1).A) /= 5 or
     Ident_Int (G.A) /= 5 or Ident_Int (H.A) /= 5 or Ident_Int (Z.A) /= 5 then
      Failed ("INCORRECT INITIALIZATION VALUES");
   end if;

   Result;
end C37404b;

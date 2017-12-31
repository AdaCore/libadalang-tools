--C37404A.ADA

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
--     CHECK THAT 'CONSTRAINED IS TRUE FOR VARIABLES DECLARED WITH A
--     CONSTRAINED TYPE, FOR CONSTANT OBJECTS (EVEN IF NOT DECLARED
--     WITH A CONSTRAINED TYPE), AND DESIGNATED OBJECTS.

-- HISTORY:
--     DHH 02/25/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C37404a is

   subtype Int is Integer range 1 .. 10;
   type Rec (A : Int) is record
      I : Int;
   end record;

   type Acc_Rec is access Rec (4);
   type Acc_Rec1 is access Rec;
   subtype Rec4 is Rec (4);
   subtype Rec5 is Rec;

   type Rec_Def (A : Int := 5) is record
      I : Int := 1;
   end record;

   type Acc_Def is access Rec_Def (4);
   type Acc_Def1 is access Rec_Def;
   subtype Rec6 is Rec_Def (6);
   subtype Rec7 is Rec_Def;

   A : Rec4     := (A => 4, I => 1);                    -- CONSTRAINED.
   B : Rec5 (4) := (A => 4, I => 1);                 -- CONSTRAINED.
   C : Rec6;                                        -- CONSTRAINED.
   D : Rec7 (6);                                     -- CONSTRAINED.
   E : Acc_Rec1 (4);                                 -- CONSTRAINED.
   F : Acc_Def1 (4);                                 -- CONSTRAINED.
   G : Acc_Rec1;                                    -- UNCONSTRAINED.
   H : Acc_Def1;                                    -- UNCONSTRAINED.

   R : Rec (5) := (A => 5, I => 1);                  -- CONSTRAINED.
   T : Rec_Def (5);                                  -- CONSTRAINED.
   U : Acc_Rec;                                     -- CONSTRAINED.
   V : Acc_Def;                                     -- CONSTRAINED.
   W : constant Rec (5)     := (A => 5, I => 1);         -- CONSTANT.
   X : constant Rec         := (A => 5, I => 1);            -- CONSTANT.
   Y : constant Rec_Def (5) := (A => 5, I => 1);     -- CONSTANT.
   Z : constant Rec_Def     := (A => 5, I => 1);        -- CONSTANT.

begin
   Test
     ("C37404A",
      "CHECK THAT 'CONSTRAINED IS TRUE FOR VARIABLES " &
      "DECLARED WITH A  CONSTRAINED TYPE, FOR " &
      "CONSTANT OBJECTS (EVEN IF NOT DECLARED WITH A " &
      "CONSTRAINED TYPE), AND DESIGNATED OBJECTS");

   U := new Rec (4);
   V := new Rec_Def (4);
   E := new Rec (4);
   F := new Rec_Def (4);
   G := new Rec (4);                                 -- CONSTRAINED.
   H := new Rec_Def (4);                             -- CONSTRAINED.

   if not A'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR SUBTYPE1");
   end if;

   if not B'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR SUBTYPE2");
   end if;

   if not C'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT SUBTYPE1");
   end if;

   if not D'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT SUBTYPE2");
   end if;

   if not R'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR RECORD COMPONENT");
   end if;

   if not T'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT VARIABLE");
   end if;

   if not E.all'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR ACCESS 1");
   end if;

   if not F.all'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT ACCESS 1");
   end if;

   if not G.all'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR ACCESS 2");
   end if;

   if not H.all'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT ACCESS 2");
   end if;

   if not U.all'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR ACCESS 3");
   end if;

   if not V.all'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT ACCESS 3");
   end if;

   if not W'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR CONSTANT, CONSTRAINED");
   end if;

   if not X'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR CONSTANT, UNCONSTRAINED");
   end if;

   if not Y'Constrained then
      Failed ("'CONSTRAINED NOT TRUE FOR DEFAULT CONSTANT, " & "CONSTRAINED");
   end if;

   if not Z'Constrained then
      Failed
        ("'CONSTRAINED NOT TRUE FOR DEFAULT CONSTANT, " & "UNCONSTRAINED");
   end if;

   if Ident_Int (T.I) /= 1 or Ident_Int (C.I) /= 1 or Ident_Int (D.I) /= 1 or
     Ident_Int (W.A) /= 5 or Ident_Int (X.A) /= 5 or Ident_Int (Y.A) /= 5 or
     Ident_Int (Z.I) /= 1 or Ident_Int (A.I) /= 1 or Ident_Int (B.I) /= 1 or
     Ident_Bool (R.I /= 1) then
      Failed ("INCORRECT INITIALIZATION VALUES");
   end if;

   Result;
end C37404a;

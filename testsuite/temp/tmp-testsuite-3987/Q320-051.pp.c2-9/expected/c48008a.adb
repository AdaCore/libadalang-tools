-- C48008A.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T X", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS AN UNCONSTRAINED RECORD, PRIVATE, OR LIMITED TYPE, X
-- IS A DISCRIMINANT CONSTRAINT, AND
--   1) ONE OF THE VALUES OF X IS OUTSIDE THE RANGE OF THE CORRESPONDING
--      DISCRIMINANT;
--   2) ONE OF THE DISCRIMINANT VALUES IS NOT COMPATIBLE WITH A
--      CONSTRAINT OF A SUBCOMPONENT IN WHICH IT IS USED;
--   3) ONE OF THE DISCRIMINANT VALUES DOES NOT EQUAL THE CORRESPONDING
--      VALUE OF THE ALLOCATOR'S BASE TYPE;
--   4) A DEFAULT INITIALIZATION RAISES AN EXCEPTION.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/02/83
-- EG  07/05/84
-- PWB 02/05/86  CORRECTED TEST ERROR:
--               CHANGED "FAILED" TO "COMMENT" IN PROCEDURE INCR_CHECK,
--               SO AS NOT TO PROHIBIT EVAL OF DEFLT EXPR (AI-00397/01)
--               ADDED COMMENTS FOR CASES.

with Report;

procedure C48008a is

   use Report;

begin

   Test
     ("C48008A",
      "FOR ALLOCATORS OF THE FORM 'NEW T X', " &
      "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - UNCONSTRAINED RECORD AND " &
      "PRIVATE TYPES");

   declare

      Disc_Flag : Boolean := False;
      Incr_Val  : Integer;
      function Incr (A : Integer) return Integer;

      subtype I1_7 is Integer range Ident_Int (1) .. Ident_Int (7);
      subtype I1_10 is Integer range Ident_Int (1) .. Ident_Int (10);
      subtype I2_9 is Integer range Ident_Int (2) .. Ident_Int (9);

      type Rec (A : I2_9) is record
         B : Integer := Incr (2);
      end record;

      type Arr is array (I2_9 range <>) of Integer;

      type T_Rec (C : I1_10) is record
         D : Rec (C);
      end record;

      type T_Arr (C : I1_10) is record
         D : Arr (2 .. C);
         E : Arr (C .. 9);
      end record;

      type T_Rec_Rec (A : I1_10) is record
         B : T_Rec (A);
      end record;

      type T_Rec_Arr (A : I1_10) is record
         B : T_Arr (A);
      end record;

      type Tb (A : I1_7) is record
         R : Integer := Incr (1);
      end record;

      type Ur (A : Integer) is record
         B : I2_9 := Incr (1);
      end record;

      type A_T_Rec_Rec is access T_Rec_Rec;
      type A_T_Rec_Arr is access T_Rec_Arr;
      type Atb is access Tb;
      type Actb is access Tb (3);
      type A_Ur is access Ur;

      Va_T_Rec_Rec : A_T_Rec_Rec;
      Va_T_Rec_Arr : A_T_Rec_Arr;
      Vb           : Atb;
      Vcb          : Actb;
      V_A_Ur       : A_Ur;

      Bool : Boolean;

      function Disc (A : Integer) return Integer;

      package P is
         type Priv (A : I1_10 := Disc (8)) is private;
         Cons_Priv : constant Priv;
      private
         type Priv (A : I1_10 := Disc (8)) is record
            R : Integer := Incr (1);
         end record;
         Cons_Priv : constant Priv := (2, 3);
      end P;

      type A_Priv is access P.Priv;
      type A_Cpriv is access P.Priv (3);

      Vp  : A_Priv;
      Vcp : A_Cpriv;

      procedure Prec_Rec (X : A_T_Rec_Rec) is
      begin
         null;
      end Prec_Rec;

      procedure Prec_Arr (X : A_T_Rec_Arr) is
      begin
         null;
      end Prec_Arr;

      procedure Pb (X : Atb) is
      begin
         null;
      end Pb;

      procedure Pcb (X : Actb) is
      begin
         null;
      end Pcb;

      procedure Ppriv (X : A_Priv) is
      begin
         null;
      end Ppriv;

      procedure Pcpriv (X : A_Cpriv) is
      begin
         null;
      end Pcpriv;

      function Disc (A : Integer) return Integer is
      begin
         Disc_Flag := True;
         return A;
      end Disc;

      function Incr (A : Integer) return Integer is
      begin
         Incr_Val := Ident_Int (Incr_Val + 1);
         return A;
      end Incr;

      procedure Incr_Check (Case_Id : String) is
      begin
         if Incr_Val /= Ident_Int (0) then
            Comment
              ("DEFAULT INITIAL VALUE WAS EVALUATED - " & "CASE " & Case_Id);
         end if;
      end Incr_Check;

   begin

      begin  -- A1A: 0 ILLEGAL FOR TB.A.
         Incr_Val := 0;
         Vb       := new Tb (A => 0);
         Failed ("NO EXCEPTION RAISED - CASE A1A");
      exception
         when Constraint_Error =>
            Incr_Check ("A1A");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A1A");
      end;   -- A1A

      begin  -- A1B: 8 ILLEGAL IN I1_7.
         Incr_Val := 0;
         Vb       := new Tb (A => I1_7'(Ident_Int (8)));
         Failed ("NO EXCEPTION RAISED - CASE A1B");
      exception
         when Constraint_Error =>
            Incr_Check ("A1B");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A1B");
      end;   -- A1B

      begin  -- A1C: 8 ILLEGAL FOR TB.A.
         Incr_Val := 0;
         Pb (new Tb (A => 8));
         Failed ("NO EXCEPTION RAISED - CASE A1C");
      exception
         when Constraint_Error =>
            Incr_Check ("A1C");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A1C");
      end;   --A1C

      begin  --A1D: 0 ILLEGAL FOR TB.A.
         Incr_Val := 0;
         Bool     := Atb'(new Tb (A => 0)) = null;
         Failed ("NO EXCEPTION RAISED - CASE A1D");
      exception
         when Constraint_Error =>
            Incr_Check ("A1D");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A1D");
      end;   --A1D

      begin  --A1E: 11 ILLEGAL FOR PRIV.A.
         Disc_Flag := False;
         Incr_Val  := 0;
         Vp        := new P.Priv (11);
         Failed ("NO EXCEPTION RAISED - CASE A1E");
      exception
         when Constraint_Error =>
            if Disc_Flag then
               Failed
                 ("DISCR DEFAULT EVALUATED WHEN " &
                  "EXPLICIT VALUE WAS PROVIDED - A1E");
            end if;
            Incr_Check ("A1E");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A1E");
      end;   -- A1E

      begin  -- A2A: 1 ILLEGAL FOR REC.A.
         Incr_Val     := 0;
         Va_T_Rec_Rec := new T_Rec_Rec (A => I1_10'(Ident_Int (1)));
         Failed ("NO EXCEPTION RAISED - CASE A2A");
      exception
         when Constraint_Error =>
            Incr_Check ("A2A");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A2A");
      end;   -- A2A

      begin  --A2B: 10 ILLEGAL FOR REC.A.
         Incr_Val     := 0;
         Va_T_Rec_Rec := new T_Rec_Rec (10);
         Failed ("NO EXCEPTION RAISED - CASE A2B");
      exception
         when Constraint_Error =>
            Incr_Check ("A2B");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A2B");
      end;   -- A2B

      begin  -- A2C: 1 ILLEGAL FOR T.ARR.E'FIRST.
         Incr_Val := 0;
         Prec_Arr (new T_Rec_Arr (1));
         Failed ("NO EXCEPTION RAISED - CASE A2C");
      exception
         when Constraint_Error =>
            Incr_Check ("A2C");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A2C");
      end;   -- A2C

      begin  -- A2D: 10 ILLEGAL FOR T_ARR.D'LAST.
         Incr_Val := 0;
         Bool     := new T_Rec_Arr (Ident_Int (10)) = null;
         Failed ("NO EXCEPTION RAISED - CASE A2D");
      exception
         when Constraint_Error =>
            Incr_Check ("A2D");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A2D");
      end;   -- A2D

      begin -- A3A: ASSIGNMENT VIOLATES CONSTRAINT ON VCB'S SUBTYPE.
         Incr_Val := 0;
         Vcb      := new Tb (4);
         Failed ("NO EXCEPTION RAISED - CASE A3A");
      exception
         when Constraint_Error =>
            Incr_Check ("A3A");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A3A");
      end;   -- A3A

      begin  -- A3B: PARM ASSOC VIOLATES CONSTRAINT ON PARM SUBTYPE.
         Incr_Val := 0;
         Pcb (new Tb (4));
         Failed ("NO EXCEPTION RAISED - CASE A3B");
      exception
         when Constraint_Error =>
            Incr_Check ("A3B");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A3B");
      end;   -- A3B

      begin  -- A3C: 2 VIOLATES CONSTRAINT ON SUBTYPE ACTB.
         Incr_Val := 0;
         Bool     := Actb'(new Tb (Ident_Int (2))) = null;
         Failed ("NO EXCEPTION RAISED - CASE A3C");
      exception
         when Constraint_Error =>
            Incr_Check ("A3C");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A3C");
      end;   -- A3C

      begin  -- A4A: EVALUATION OF DEFAULT RAISES EXCEPTION.
         Incr_Val := 0;
         V_A_Ur   := new Ur (4);
         Failed ("NO EXCEPTION RAISED - CASE A4A");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CASE A4A");
      end;   -- A4A

   end;

   Result;

end C48008a;

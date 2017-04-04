-- C46051A.ADA

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
-- CHECK THAT ENUMERATION, RECORD, ACCESS, PRIVATE, AND TASK VALUES CAN BE
-- CONVERTED IF THE OPERAND AND TARGET TYPES ARE RELATED BY DERIVATION.

-- R.WILLIAMS 9/8/86

with Report; use Report;
procedure C46051a is

begin
   Test
     ("C46051A",
      "CHECK THAT ENUMERATION, RECORD, ACCESS, " &
      "PRIVATE, AND TASK VALUES CAN BE CONVERTED " &
      "IF THE OPERAND AND TARGET TYPES ARE " &
      "RELATED BY DERIVATION");

   declare
      type Enum is (A, Ab, Abc, Abcd);
      E : Enum := Abc;

      type Enum1 is new Enum;
      E1 : Enum1 := Enum1'Val (Ident_Int (2));

      type Enum2 is new Enum;
      E2 : Enum2 := Abc;

      type Nenum1 is new Enum1;
      Ne : Nenum1 := Nenum1'Val (Ident_Int (2));
   begin
      if Enum (E) /= E then
         Failed ("INCORRECT CONVERSION OF 'ENUM (E)'");
      end if;

      if Enum (E1) /= E then
         Failed ("INCORRECT CONVERSION OF 'ENUM (E1)'");
      end if;

      if Enum1 (E2) /= E1 then
         Failed ("INCORRECT CONVERSION OF 'ENUM1 (E2)'");
      end if;

      if Enum2 (Ne) /= E2 then
         Failed ("INCORRECT CONVERSION OF 'ENUM2 (NE)'");
      end if;

      if Nenum1 (E) /= Ne then
         Failed ("INCORRECT CONVERSION OF 'NENUM (E)'");
      end if;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED DURING CONVERSION OF " & "ENUMERATION TYPES");
   end;

   declare
      type Rec is record
         null;
      end record;

      R : Rec;

      type Rec1 is new Rec;
      R1 : Rec1;

      type Rec2 is new Rec;
      R2 : Rec2;

      type Nrec1 is new Rec1;
      Nr : Nrec1;
   begin
      if Rec (R) /= R then
         Failed ("INCORRECT CONVERSION OF 'REC (R)'");
      end if;

      if Rec (R1) /= R then
         Failed ("INCORRECT CONVERSION OF 'REC (R1)'");
      end if;

      if Rec1 (R2) /= R1 then
         Failed ("INCORRECT CONVERSION OF 'REC1 (R2)'");
      end if;

      if Rec2 (Nr) /= R2 then
         Failed ("INCORRECT CONVERSION OF 'REC2 (NR)'");
      end if;

      if Nrec1 (R) /= Nr then
         Failed ("INCORRECT CONVERSION OF 'NREC (R)'");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING CONVERSION OF " & "RECORD TYPES");
   end;

   declare
      type Rec (D : Integer) is record
         null;
      end record;

      subtype Crec is Rec (3);
      R : Crec;

      type Crec1 is new Rec (3);
      R1 : Crec1;

      type Crec2 is new Rec (3);
      R2 : Crec2;

      type Ncrec1 is new Crec1;
      Nr : Ncrec1;
   begin
      if Crec (R) /= R then
         Failed ("INCORRECT CONVERSION OF 'CREC (R)'");
      end if;

      if Crec (R1) /= R then
         Failed ("INCORRECT CONVERSION OF 'CREC (R1)'");
      end if;

      if Crec1 (R2) /= R1 then
         Failed ("INCORRECT CONVERSION OF 'CREC1 (R2)'");
      end if;

      if Crec2 (Nr) /= R2 then
         Failed ("INCORRECT CONVERSION OF 'CREC2 (NR)'");
      end if;

      if Ncrec1 (R) /= Nr then
         Failed ("INCORRECT CONVERSION OF 'NCREC (R)'");
      end if;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED DURING CONVERSION OF " &
            "RECORD TYPES WITH DISCRIMINANTS");
   end;

   declare
      type Rec is record
         null;
      end record;

      type Accrec is access Rec;
      Ar : Accrec;

      type Accrec1 is new Accrec;
      Ar1 : Accrec1;

      type Accrec2 is new Accrec;
      Ar2 : Accrec2;

      type Naccrec1 is new Accrec1;
      Nar : Naccrec1;

      function F (A : Accrec) return Integer is
      begin
         return Ident_Int (0);
      end F;

      function F (A : Accrec1) return Integer is
      begin
         return Ident_Int (1);
      end F;

      function F (A : Accrec2) return Integer is
      begin
         return Ident_Int (2);
      end F;

      function F (A : Naccrec1) return Integer is
      begin
         return Ident_Int (3);
      end F;

   begin
      if F (Accrec (Ar)) /= 0 then
         Failed ("INCORRECT CONVERSION OF 'ACCREC (AR)'");
      end if;

      if F (Accrec (Ar1)) /= 0 then
         Failed ("INCORRECT CONVERSION OF 'ACCREC (AR1)'");
      end if;

      if F (Accrec1 (Ar2)) /= 1 then
         Failed ("INCORRECT CONVERSION OF 'ACCREC1 (AR2)'");
      end if;

      if F (Accrec2 (Nar)) /= 2 then
         Failed ("INCORRECT CONVERSION OF 'ACCREC2 (NAR)'");
      end if;

      if F (Naccrec1 (Ar)) /= 3 then
         Failed ("INCORRECT CONVERSION OF 'NACCREC (AR)'");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING CONVERSION OF " & "ACCESS TYPES");
   end;

   declare
      type Rec (D : Integer) is record
         null;
      end record;

      type Accr is access Rec;

      subtype Caccr is Accr (3);
      Ar : Caccr;

      type Caccr1 is new Accr (3);
      Ar1 : Caccr1;

      type Caccr2 is new Accr (3);
      Ar2 : Caccr2;

      type Ncaccr1 is new Caccr1;
      Nar : Ncaccr1;

      function F (A : Caccr) return Integer is
      begin
         return Ident_Int (0);
      end F;

      function F (A : Caccr1) return Integer is
      begin
         return Ident_Int (1);
      end F;

      function F (A : Caccr2) return Integer is
      begin
         return Ident_Int (2);
      end F;

      function F (A : Ncaccr1) return Integer is
      begin
         return Ident_Int (3);
      end F;

   begin
      if F (Caccr (Ar)) /= 0 then
         Failed ("INCORRECT CONVERSION OF 'CACCR (AR)'");
      end if;

      if F (Caccr (Ar1)) /= 0 then
         Failed ("INCORRECT CONVERSION OF 'CACCR (AR1)'");
      end if;

      if F (Caccr1 (Ar2)) /= 1 then
         Failed ("INCORRECT CONVERSION OF 'CACCR1 (AR2)'");
      end if;

      if F (Caccr2 (Nar)) /= 2 then
         Failed ("INCORRECT CONVERSION OF 'CACCR2 (NAR)'");
      end if;

      if F (Ncaccr1 (Ar)) /= 3 then
         Failed ("INCORRECT CONVERSION OF 'NCACCR (AR)'");
      end if;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED DURING CONVERSION OF " &
            "CONSTRAINED ACCESS TYPES");
   end;

   declare
      package Pkg1 is
         type Priv is private;
      private
         type Priv is record
            null;
         end record;
      end Pkg1;

      use Pkg1;

      package Pkg2 is
         R : Priv;

         type Priv1 is new Priv;
         R1 : Priv1;

         type Priv2 is new Priv;
         R2 : Priv2;
      end Pkg2;

      use Pkg2;

      package Pkg3 is
         type Npriv1 is new Priv1;
         Nr : Npriv1;
      end Pkg3;

      use Pkg3;
   begin
      if Priv (R) /= R then
         Failed ("INCORRECT CONVERSION OF 'PRIV (R)'");
      end if;

      if Priv (R1) /= R then
         Failed ("INCORRECT CONVERSION OF 'PRIV (R1)'");
      end if;

      if Priv1 (R2) /= R1 then
         Failed ("INCORRECT CONVERSION OF 'PRIV1 (R2)'");
      end if;

      if Priv2 (Nr) /= R2 then
         Failed ("INCORRECT CONVERSION OF 'PRIV2 (NR)'");
      end if;

      if Npriv1 (R) /= Nr then
         Failed ("INCORRECT CONVERSION OF 'NPRIV (R)'");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING CONVERSION OF " & "PRIVATE TYPES");
   end;

   declare
      task type Tk;
      T : Tk;

      type Tk1 is new Tk;
      T1 : Tk1;

      type Tk2 is new Tk;
      T2 : Tk2;

      type Ntk1 is new Tk1;
      Nt : Ntk1;

      task body Tk is
      begin
         null;
      end Tk;

      function F (T : Tk) return Integer is
      begin
         return Ident_Int (0);
      end F;

      function F (T : Tk1) return Integer is
      begin
         return Ident_Int (1);
      end F;

      function F (T : Tk2) return Integer is
      begin
         return Ident_Int (2);
      end F;

      function F (T : Ntk1) return Integer is
      begin
         return Ident_Int (3);
      end F;

   begin
      if F (Tk (T)) /= 0 then
         Failed ("INCORRECT CONVERSION OF 'TK (T))'");
      end if;

      if F (Tk (T1)) /= 0 then
         Failed ("INCORRECT CONVERSION OF 'TK (T1))'");
      end if;

      if F (Tk1 (T2)) /= 1 then
         Failed ("INCORRECT CONVERSION OF 'TK1 (T2))'");
      end if;

      if F (Tk2 (Nt)) /= 2 then
         Failed ("INCORRECT CONVERSION OF 'TK2 (NT))'");
      end if;

      if F (Ntk1 (T)) /= 3 then
         Failed ("INCORRECT CONVERSION OF 'NTK (T))'");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING CONVERSION OF " & "TASK TYPES");
   end;

   Result;
end C46051a;

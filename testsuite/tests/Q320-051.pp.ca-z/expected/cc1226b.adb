-- CC1226B.ADA

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
--     CHECK, FOR A FORMAL NONLIMITED PRIVATE TYPE, THAT ALL ALLOWABLE
--     OPERATIONS ARE IMPLICITLY DECLARED.

-- HISTORY:
--     BCB 04/04/88  CREATED ORIGINAL TEST.
--     RJW 03/28/90  INITIALIZED PREVIOUSLY UNINITIALIZED VARIABLES.
--     LDC 09/19/90  INITALIZED NLPVAR & NLPVAR2 TO DIFFERENT VALUES,
--                   REMOVED USE CLAUSE.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
with System; use System;

procedure Cc1226b is

   type Discrec (Disc1 : Integer := 1; Disc2 : Boolean := False) is record
      null;
   end record;

   generic
      type Nlp is private;
      type Nlpdisc (Disc1 : Integer; Disc2 : Boolean) is private;
      with procedure Initialize (N : out Nlpdisc);
      with function Initialize return Nlp;
      with function Initialize_2 return Nlp;
   package P is
      function Ident (X : Nlp) return Nlp;
      function Ident_Adr (Y : Address) return Address;
   end P;

   package body P is
      type Der_Nlp is new Nlp;
      Nlpvar           : Nlp     := Initialize_2;
      Nlpvar2, Nlpvar3 : Nlp     := Initialize;
      Dernlp           : Der_Nlp := Der_Nlp (Initialize);
      Ndvar            : Nlpdisc (Disc1 => 5, Disc2 => True);
      Nlpvaraddress    : Address;
      Nlpsize          : Integer;
      Nlpbasesize      : Integer;

      function Ident (X : Nlp) return Nlp is
         Z : Nlp := Initialize;
      begin
         if Equal (3, 3) then
            return X;
         end if;
         return Z;
      end Ident;

      function Ident_Adr (Y : Address) return Address is
         I : Integer;
         Z : Address := I'Address;
      begin
         if Equal (3, 3) then
            return Y;
         end if;
         return Z;
      end Ident_Adr;

   begin
      Test
        ("CC1226B",
         "CHECK, FOR A FORMAL NONLIMITED PRIVATE " &
         "TYPE THAT ALL ALLOWABLE OPERATIONS ARE " & "IMPLICITLY DECLARED");

      Initialize (Ndvar);

      Nlpvar := Nlpvar2;

      if Nlpvar /= Nlpvar2 then
         Failed ("IMPROPER VALUE FROM ASSIGNMENT");
      end if;

      if Nlpvar not in Nlp then
         Failed ("IMPROPER RESULT FROM MEMBERSHIP TEST");
      end if;

      Nlpvar := Nlp'(Nlpvar2);

      if Nlpvar /= Nlpvar2 then
         Failed ("IMPROPER RESULT FROM QUALIFICATION");
      end if;

      Nlpvar := Nlp (Dernlp);

      if Nlpvar /= Ident (Nlp (Dernlp)) then
         Failed ("IMPROPER RESULT FROM EXPLICIT CONVERSION");
      end if;

      Nlpsize := Ident_Int (Nlp'Size);

      if Nlpsize /= Integer (Nlp'Size) then
         Failed ("IMPROPER VALUE FOR NLP'SIZE");
      end if;

      Nlpvaraddress := Nlpvar'Address;

      if Nlpvar'Address /= Ident_Adr (Nlpvaraddress) then
         Failed ("IMPROPER VALUE FOR NLPVAR'ADDRESS");
      end if;

      if Ndvar.Disc1 /= Ident_Int (5) then
         Failed ("IMPROPER DISCRIMINANT VALUE - 1");
      end if;

      if not Ndvar.Disc2 then
         Failed ("IMPROPER DISCRIMINANT VALUE - 2");
      end if;

      if not Ndvar'Constrained then
         Failed ("IMPROPER VALUE FOR NDVAR'CONSTRAINED");
      end if;

      Nlpvar := Nlpvar3;

      if not (Nlpvar = Ident (Nlpvar3)) then
         Failed ("IMPROPER VALUE FROM EQUALITY OPERATION");
      end if;

      if Nlpvar /= Ident (Nlpvar3) then
         Failed ("IMPROPER VALUE FROM INEQUALITY OPERATION");
      end if;

      Result;
   end P;

   procedure Initialize (I : out Discrec) is
   begin
      I := (5, True);
   end Initialize;

   function Initialize return Integer is
   begin
      return 5;
   end Initialize;

   function Initialize_Other return Integer is
   begin
      return 3;
   end Initialize_Other;

   package Pack is new P (Integer, Discrec, Initialize, Initialize,
      Initialize_Other);

begin
   null;
end Cc1226b;

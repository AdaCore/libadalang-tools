-- C32001E.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR PRIVATE TYPES, THE SUBTYPE
-- INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED ONCE FOR EACH
-- NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE INDICATION IS EVALUATED FIRST.
-- ALSO, CHECK THAT THE EVALUATIONS YIELD THE SAME RESULT AS A SEQUENCE OF
-- SINGLE OBJECT DECLARATIONS.

-- RJW 7/18/86

with Report; use Report;

procedure C32001e is

   Bump : array (1 .. 10) of Integer := (others => 0);
   G1   : array (5 .. 6) of Integer;

   function F (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      return Bump (I);
   end F;

   function G (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      G1 (I)   := Bump (I);
      return Bump (I);
   end G;

begin
   Test
     ("C32001E",
      "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " &
      "FOR PRIVATE TYPES, THE SUBTYPE INDICATION " &
      "AND THE INITIALIZATION EXPRESSIONS ARE " &
      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
      "EVALUATIONS YIELD THE SAME RESULT AS A " &
      "SEQUENCE OF SINGLE OBJECT DECLARATIONS");

   declare
      package Pkg1 is
         type Pbool is private;
         type Pint is private;
         type Prec (D : Integer) is private;
         type Parr is private;
         type Pacc is private;

         function Init1 (I : Integer) return Pbool;
         function Init2 (I : Integer) return Pint;
         function Init3 (I : Integer) return Prec;
         function Init4 (I : Integer) return Parr;
         function Init5 (I : Integer) return Pacc;

         procedure Check1 (B : Pbool; I : Integer; S : String);
         procedure Check2 (I : Pint; J : Integer; S : String);
         procedure Check3 (R : Prec; I, J : Integer; S : String);
         procedure Check4 (A : Parr; I, J : Integer; S : String);
         procedure Check5 (V : Pacc; S : String);
         procedure Check6 (V : Pacc; S : String);

      private
         type Pbool is new Boolean;
         type Pint is new Integer;

         type Prec (D : Integer) is record
            Value : Integer;
         end record;

         type Parr is array (1 .. 2) of Integer;

         type Vector is array (Natural range <>) of Integer;
         type Pacc is access Vector;
      end Pkg1;

      package body Pkg1 is
         function Init1 (I : Integer) return Pbool is
         begin
            return Pbool'Val (F (I) - 1);
         end Init1;

         function Init2 (I : Integer) return Pint is
         begin
            return Pint'Val (F (I));
         end Init2;

         function Init3 (I : Integer) return Prec is
            Pr : Prec (G1 (I)) := (G1 (I), F (I));
         begin
            return Pr;
         end Init3;

         function Init4 (I : Integer) return Parr is
            Pa : Parr := (1 .. 2 => F (I));
         begin
            return Pa;
         end Init4;

         function Init5 (I : Integer) return Pacc is
            Accv : Pacc := new Vector'(1 .. F (I) => F (I));
         begin
            return Accv;
         end Init5;

         procedure Check1 (B : Pbool; I : Integer; S : String) is
         begin
            if B /= Pbool'Val (I) then
               Failed (S & " HAS AN INCORRECT VALUE OF " & Pbool'Image (B));
            end if;
         end Check1;

         procedure Check2 (I : Pint; J : Integer; S : String) is
         begin
            if I /= Pint'Val (J) then
               Failed (S & " HAS AN INCORRECT VALUE OF " & Pint'Image (I));
            end if;
         end Check2;

         procedure Check3 (R : Prec; I, J : Integer; S : String) is
         begin
            if R.D /= I then
               Failed
                 (S & ".D HAS AN INCORRECT VALUE OF " & Integer'Image (R.D));
            end if;

            if R.Value /= J then
               Failed
                 (S & ".VALUE HAS AN INCORRECT " & "VALUE OF " &
                  Integer'Image (R.Value));
            end if;
         end Check3;

         procedure Check4 (A : Parr; I, J : Integer; S : String) is
         begin
            if A /= (I, J) and A /= (J, I) then
               Failed (S & " HAS AN INCORRECT VALUE");
            end if;
         end Check4;

         procedure Check5 (V : Pacc; S : String) is
         begin
            if V'Last /= 1 then
               Failed
                 (S & " HAS AN INCORRECT UPPER BOUND " & "OF " &
                  Integer'Image (V'Last));
            end if;

            if V (1) /= 2 then
               Failed (S & " HAS AN INCORRECT COMPONENT " & "VALUE");
            end if;
         end Check5;

         procedure Check6 (V : Pacc; S : String) is
         begin
            if V'Last /= 3 then
               Failed
                 (S & " HAS AN INCORRECT UPPER BOUND " & "OF " &
                  Integer'Image (V'Last));
            end if;

            if V.all = (4, 5, 6) or V.all = (5, 4, 6) or V.all = (4, 6, 5) or
              V.all = (6, 4, 5) or V.all = (5, 6, 4) or V.all = (6, 5, 4) then
               null;
            else
               Failed (S & " HAS AN INCORRECT COMPONENT " & "VALUE");
            end if;
         end Check6;

      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

         B1, B2   : Pbool          := Init1 (1);
         Cb1, Cb2 : constant Pbool := Init1 (2);

         I1, I2   : Pint          := Init2 (3);
         Ci1, Ci2 : constant Pint := Init2 (4);

         R1, R2   : Prec (G (5))          := Init3 (5);
         Cr1, Cr2 : constant Prec (G (6)) := Init3 (6);

         A1, A2   : Parr          := Init4 (7);
         Ca1, Ca2 : constant Parr := Init4 (8);

         V1, V2   : Pacc          := Init5 (9);
         Cv1, Cv2 : constant Pacc := Init5 (10);

      begin
         Check1 (B1, 0, "B1");
         Check1 (B2, 1, "B2");
         Check1 (Cb1, 0, "CB1");
         Check1 (Cb2, 1, "CB2");

         Check2 (I1, 1, "I1");
         Check2 (I2, 2, "I2");
         Check2 (Ci1, 1, "CI1");
         Check2 (Ci2, 2, "CI2");

         Check3 (R1, 1, 2, "R1");
         Check3 (R2, 3, 4, "R2");
         Check3 (Cr1, 1, 2, "CR1");
         Check3 (Cr2, 3, 4, "CR2");

         Check4 (A1, 1, 2, "A1");
         Check4 (A2, 3, 4, "A2");
         Check4 (Ca1, 1, 2, "CA1");
         Check4 (Ca2, 3, 4, "CA2");

         Check5 (V1, "V1");
         Check6 (V2, "V2");
         Check5 (Cv1, "CV1");
         Check6 (Cv2, "CV2");
      end Pkg2;

   begin
      null;
   end;

   Result;
end C32001e;

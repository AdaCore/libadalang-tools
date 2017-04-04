-- C51004A.ADA

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
-- CHECK THAT LABELS, LOOP IDENTIFIERS, AND BLOCK IDENTIFIERS ARE IMPLICITLY
-- DECLARED AT THE END OF THE DECLARATIVE PART. PRIOR TO THE END OF THE
-- DECLARATIVE PART, THEY MAY BE USED TO REFERENCE ENTITIES IN AN ENCLOSING
-- SCOPE. SUBTESTS ARE:
--        (A) BLOCK.
--        (B) PROCEDURE BODY.
--        (C) PACKAGE BODY.
--        (D) GENERIC FUNCTION BODY.
--        (E) GENERIC PACKAGE BODY.
--        (F) TASK BODY.

-- CPP  6/1/84

with Report; use Report;
procedure C51004a is

begin
   Test
     ("C51004A",
      "CHECK THAT LABELS, LOOP IDENTIFIERS, AND BLOCK " &
      "IDENTIFIERS MAY BE USED PRIOR TO THEIR IMPLICIT " &
      "DECLARATION");

   Outer : declare

      type Idn1 is new Integer;
      Idn2 : constant Integer := 2;
      type Idn3 is access Integer;

   begin     -- OUTER

      -----------------------------------------------

      A : declare

         A1 : Idn1;
         A2 : constant Integer := Idn2;
         A3 : Idn3;

         Temp : Integer;

      begin     -- A

         <<IDN1>>
         Temp := 0;

         Idn2 :
         for I in 1 .. 1 loop
            Temp := A2;
         end loop Idn2;

         Idn3 : begin
            null;
         end Idn3;

      end A;

      -----------------------------------------------

      B : declare

         procedure P (Temp : out Integer) is

            B1 : Idn1;
            B2 : constant Integer := Idn2 + 2;
            B3 : Idn3;

         begin     -- P

            <<L>>
            <<IDN1>>
            Temp := 0;

            Idn2 :
            while B2 < 0 loop
               Temp := 0;
            end loop Idn2;

            Idn3 : declare
            begin
               null;
            end Idn3;

         end P;

      begin     -- B
         null;
      end B;

      -----------------------------------------------

      C : declare

         package Pkg is
         end Pkg;

         package body Pkg is

            C1 : Idn1;
            C2 : constant Integer := 2 * Idn2;
            C3 : Idn3;

            Temp : Integer;

         begin

            <<IDN1>>
            Temp := 0;

            Idn2 :
            loop
               Temp := 0;
               exit;
            end loop Idn2;

            Idn3 : begin
               null;
            end Idn3;

         end Pkg;

      begin     -- C
         null;
      end C;

      ---------------------------------------------------

      D : declare

         generic
            type Q is (<>);
         function Fn return Integer;

         function Fn return Integer is

            D1 : Idn1;
            D2 : constant Integer := Idn2;
            D3 : Idn3;

            Temp : Integer;

         begin

            <<IDN1>>
            Temp := 0;

            Idn2 :
            for I in 1 .. 5 loop
               Temp := 0;
            end loop Idn2;

            Idn3 : begin
               null;
            end Idn3;

            return Temp;

         end Fn;

      begin
         null;
      end D;

      -----------------------------------------------

      E : declare

         generic

            type Element is (<>);
            Item : Element;

         package Pkg is
         end Pkg;

         package body Pkg is

            E1 : Idn1 range 1 .. 5;
            E2 : constant Integer := Idn2;
            E3 : Idn3;

            Temp : Element;

         begin

            <<IDN1>>
            <<L>>
            Temp := Item;

            Idn2 :
            while Temp /= Item loop
               Temp := Item;
            end loop Idn2;

            Idn3 : declare
            begin
               null;
            end Idn3;

         end Pkg;

      begin     -- E

         declare
            package P1 is new Pkg (Integer, 0);
         begin
            null;
         end;

      end E;

      -----------------------------------------------

      F : declare

         task T;

         task body T is

            F1 : Idn1 range -4 .. 2;
            F2 : constant Integer := Idn2;
            F3 : Idn3;

            Temp : Integer;

         begin

            <<IDN1>>
            Temp := 1;

            Idn2 :
            loop
               Temp := Temp + 1;
               exit;
            end loop Idn2;

            Idn3 : declare
            begin
               Temp := Temp + 1;
            end Idn3;

         end T;

      begin     -- F
         null;
      end F;

      -----------------------------------------------

   end Outer;

   Result;
end C51004a;

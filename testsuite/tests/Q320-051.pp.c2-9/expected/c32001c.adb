-- C32001C.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR RECORD TYPES, THE SUBTYPE
-- INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED ONCE FOR EACH
-- NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE INDICATION IS EVALUATED FIRST.
-- ALSO, CHECK THAT THE EVALUATIONS YIELD THE SAME RESULT AS A SEQUENCE OF
-- SINGLE OBJECT DECLARATIONS.

-- RJW 7/16/86

with Report; use Report;

procedure C32001c is

   type Arr is array (1 .. 2) of Integer;
   F1, G1 : Arr;
   Bump   : Arr := (0, 0);

   function F (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      F1 (I)   := Bump (I);
      return Bump (I);
   end F;

   function G (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      G1 (I)   := Bump (I);
      return Bump (I);
   end G;

   function H (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      return Bump (I);
   end H;

begin
   Test
     ("C32001C",
      "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " &
      "FOR RECORD TYPES, THE SUBTYPE INDICATION " &
      "AND THE INITIALIZATION EXPRESSIONS ARE " &
      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
      "EVALUATIONS YIELD THE SAME RESULT AS A " &
      "SEQUENCE OF SINGLE OBJECT DECLARATIONS");

   declare

      type Rec (D1, D2 : Integer) is record
         Value : Integer;
      end record;

      R1, R2   : Rec (F (1), G (1)) := (F1 (1), G1 (1), Value => H (1));
      Cr1, Cr2 : constant Rec (F (2), G (2)) :=
        (F1 (2), G1 (2), Value => H (2));

      procedure Check (R : Rec; V1, V2, Val : Integer; S : String) is
      begin
         if R.D1 = V1 then
            if R.D2 = V2 then
               Comment
                 (S & ".D1 INITIALIZED TO " & Integer'Image (V1) & " AND " &
                  S & ".D2 INITIALIZED TO " & Integer'Image (V2));
            else
               Failed (S & ".D2 INITIALIZED INCORRECTLY - 1");
            end if;
         elsif R.D1 = V2 then
            if R.D2 = V1 then
               Comment
                 (S & ".D1 INITIALIZED TO " & Integer'Image (V2) & " AND " &
                  S & ".D2 INITIALIZED TO " & Integer'Image (V1));
            else
               Failed (S & ".D2 INITIALIZED INCORRECTLY - 2");
            end if;
         else
            Failed
              (S & ".D1 INITIALIZED INCORRECTLY TO " & Integer'Image (R.D1));
         end if;

         if R.Value /= Val then
            Failed (S & ".VALUE INITIALIZED INCORRECTLY");
         end if;
      end Check;

   begin
      Check (R1, 1, 2, 3, "R1");
      Check (R2, 4, 5, 6, "R2");

      Check (Cr1, 1, 2, 3, "CR1");
      Check (Cr2, 4, 5, 6, "CR2");
   end;

   Result;
end C32001c;

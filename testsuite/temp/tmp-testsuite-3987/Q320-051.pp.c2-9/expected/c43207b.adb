-- C43207B.ADA

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
-- FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM (F..G => (H..I => J)),
-- CHECK THAT:

--     B) IF H..I IS A NULL RANGE, CONSTRAINT_ERROR IS RAISED IF
--        F..G IS NON-NULL AND F OR G DO NOT BELONG TO THE INDEX
--        SUBTYPE;

-- EG  01/18/84
-- BHS  7/13/84
-- JBG 12/6/84

with Report;

procedure C43207b is

   use Report;

begin

   Test
     ("C43207B",
      "CHECK THAT THE EVALUATION OF A MULTI" &
      "DIMENSIONAL AGGREGATE OF THE FORM " &
      "(F..G => (H..I = J)) IS PERFORMED " &
      "CORRECTLY");

   declare

      type Choice_Index is (F, G, H, I, J);
      type Choice_Cntr is array (Choice_Index) of Integer;

      Cntr : Choice_Cntr := (Choice_Index => 0);

      subtype Sint is Integer range 1 .. 8;
      type T0 is array (Sint range <>, Sint range <>) of Integer;

      function Calc (A : Choice_Index; B : Integer) return Integer is
      begin
         Cntr (A) := Cntr (A) + 1;
         return Ident_Int (B);
      end Calc;

   begin

      Case_B : declare
         procedure Check (A : T0; M : String) is
         begin
            if (A'First (1) /= 1) or
              (A'Last (1) /= 9) or
              (A'First (2) /= 6) or
              (A'Last (2) /= 5)
            then
               Failed ("CASE B" & M & " : ARRAY NOT " & "BOUNDED CORRECTLY");
            end if;
         end Check;
      begin

         Case_B1 : begin
            Check ((1 .. 9 => (6 .. 5 => 2)), "1");
            Failed ("CASE B1 : CONSTRAINT_ERROR NOT RAISED");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("CASE B1 : EXCEPTION RAISED");
         end Case_B1;

         Case_B2 : begin
            Check ((Calc (F, 1) .. Calc (G, 9) => (6 .. 5 => 2)), "2");
            Failed ("CASE B2 : CONSTRAINT_ERROR NOT RAISED");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("CASE B2 : EXCEPTION RAISED");
         end Case_B2;

         Case_B3 : begin
            Check ((1 .. 9 => (Calc (H, 6) .. Calc (I, 5) => 2)), "3");
            Failed ("CASE B3 : CONSTRAINT_ERROR NOT RAISED");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("CASE B3 : EXCEPTION RAISED");
         end Case_B3;

      end Case_B;

      if Cntr (F) /= 1 then
         Failed
           ("CASE B2 : F WAS NOT EVALUATED " &
            "ONCE.  F WAS EVALUATED" &
            Integer'Image (Cntr (F)) &
            " TIMES");
      end if;
      if Cntr (G) /= 1 then
         Failed
           ("CASE B2 : G WAS NOT EVALUATED " &
            "ONCE.  G WAS EVALUATED" &
            Integer'Image (Cntr (G)) &
            " TIMES");
      end if;

      if Cntr (H) /= 0 and Cntr (I) /= 0 then
         Comment
           ("CASE B3 : ALL CHOICES " &
            "EVALUATED BEFORE CHECKING " &
            "INDEX SUBTYPE");
      elsif Cntr (H) = 0 and Cntr (I) = 0 then
         Comment
           ("CASE B3 : SUBTYPE CHECKS " & "MADE AS CHOICES ARE EVALUATED");
      end if;

      if Cntr (H) > 1 then
         Failed
           ("CASE B3 : H WAS NOT EVALUATED " &
            "AT MOST ONCE. H WAS EVALUATED" &
            Integer'Image (Cntr (H)) &
            " TIMES");
      end if;

      if Cntr (I) > 1 then
         Failed
           ("CASE B3 : I WAS NOT EVALUATED " &
            "AT MOST ONCE. I WAS EVALUATED" &
            Integer'Image (Cntr (I)) &
            " TIMES");
      end if;

   end;

   Result;

end C43207b;

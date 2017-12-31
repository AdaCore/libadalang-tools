-- C43208B.ADA

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
-- FOR AN AGGREGATE OF THE FORM:
--        (B..C => (D..E => (F..G => (H..I => J))))
-- WHOSE TYPE IS A TWO-DIMENSIONAL ARRAY TYPE THAT HAS A TWO- DIMENSIONAL ARRAY
-- COMPONENT TYPE, CHECK THAT:

--     A) IF B..C OR D..E IS A NULL RANGE, THEN F, G, H, I, AND J
--        ARE NOT EVALUATED.

--     B) IF B..C AND D..E ARE NON-NULL RANGES, THEN F, G, H AND I
--        ARE EVALUATED (C-B+1)*(E-D+1) TIMES, AND J IS EVALUATED
--        (C-B+1)*(E-D+1)*(G-F+1)*(I-H+1) TIMES IF F..G AND H..I
--        ARE NON-NULL.

-- EG  01/19/84
-- RLB 09/20/07 - Corrected spelling in messages.

with Report;

procedure C43208b is

   use Report;

begin

   Test
     ("C43208B",
      "CHECK THAT THE EVALUATION OF A MULTI" &
      "DIMENSIONAL ARRAY TYPE THAT HAS AN " &
      "ARRAY COMPONENT TYPE IS PERFORMED " & "CORRECTLY");

   declare

      type Choice_Index is (B, C, D, E, F, G, H, I, J);
      type Choice_Cntr is array (Choice_Index) of Integer;

      Cntr : Choice_Cntr := (Choice_Index => 0);

      type T1 is array (Integer range <>, Integer range <>) of Integer;

      function Calc (A : Choice_Index; B : Integer) return Integer is
      begin
         Cntr (A) := Cntr (A) + 1;
         return Ident_Int (B);
      end Calc;

   begin

      Case_A :
      begin

         Case_A1 :
         declare
            A1 : array (4 .. 3, 3 .. 4) of T1 (2 .. 3, 1 .. 2);
         begin
            Cntr := (Choice_Index => 0);
            A1   :=
              (4 .. 3 =>
                 (3 .. 4 =>
                    (Calc (F, 2) .. Calc (G, 3) =>
                       (Calc (H, 1) .. Calc (I, 2) => Calc (J, 2)))));
            if Cntr (F) /= 0 then
               Failed ("CASE A1 : F WAS EVALUATED");
            end if;
            if Cntr (G) /= 0 then
               Failed ("CASE A1 : G WAS EVALUATED");
            end if;
            if Cntr (H) /= 0 then
               Failed ("CASE A1 : H WAS EVALUATED");
            end if;
            if Cntr (I) /= 0 then
               Failed ("CASE A1 : I WAS EVALUATED");
            end if;
            if Cntr (J) /= 0 then
               Failed ("CASE A1 : J WAS EVALUATED");
            end if;
         exception
            when others =>
               Failed ("CASE A1 : Exception raised");
         end Case_A1;

         Case_A2 :
         declare
            A2 : array (3 .. 4, 4 .. 3) of T1 (2 .. 3, 1 .. 2);
         begin
            Cntr := (Choice_Index => 0);
            A2   :=
              (Calc (B, 3) .. Calc (C, 4) =>
                 (Calc (D, 4) .. Calc (E, 3) =>
                    (Calc (F, 2) .. Calc (G, 3) =>
                       (Calc (H, 1) .. Calc (I, 2) => Calc (J, 2)))));
            if Cntr (F) /= 0 then
               Failed ("CASE A2 : F WAS EVALUATED");
            end if;
            if Cntr (G) /= 0 then
               Failed ("CASE A2 : G WAS EVALUATED");
            end if;
            if Cntr (H) /= 0 then
               Failed ("CASE A2 : H WAS EVALUATED");
            end if;
            if Cntr (I) /= 0 then
               Failed ("CASE A2 : I WAS EVALUATED");
            end if;
            if Cntr (J) /= 0 then
               Failed ("CASE A2 : J WAS EVALUATED");
            end if;
         exception
            when others =>
               Failed ("CASE A2 : Exception raised");
         end Case_A2;

      end Case_A;

      Case_B :
      begin

         Case_B1 :
         declare
            B1 : array (2 .. 3, 1 .. 2) of T1 (1 .. 2, 9 .. 10);
         begin
            Cntr := (Choice_Index => 0);
            B1   :=
              (2 .. 3 =>
                 (1 .. 2 =>
                    (Calc (F, 1) .. Calc (G, 2) =>
                       (Calc (H, 9) .. Calc (I, 10) => Calc (J, 2)))));
            if Cntr (F) /= 4 then
               Failed ("CASE B1 : F NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (G) /= 4 then
               Failed ("CASE B1 : G NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (H) /= 4 then
               Failed ("CASE B1 : H NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (I) /= 4 then
               Failed ("CASE B1 : I NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (J) /= 16 then
               Failed
                 ("CASE B1 : J NOT EVALUATED (C-B+1)*" &
                  "(E-D+1)*(G-F+1)*(I-H+1) TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE B1 : Exception raised");
         end Case_B1;

         Case_B2 :
         declare
            B2 : array (2 .. 3, 1 .. 2) of T1 (1 .. 2, 9 .. 10);
         begin
            Cntr := (Choice_Index => 0);
            B2   :=
              (Calc (B, 2) .. Calc (C, 3) =>
                 (Calc (D, 1) .. Calc (E, 2) =>
                    (Calc (F, 1) .. Calc (G, 2) =>
                       (Calc (H, 9) .. Calc (I, 10) => Calc (J, 2)))));
            if Cntr (F) /= 4 then
               Failed ("CASE B2 : F NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (G) /= 4 then
               Failed ("CASE B2 : G NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (H) /= 4 then
               Failed ("CASE B2 : H NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (I) /= 4 then
               Failed ("CASE B2 : I NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (J) /= 16 then
               Failed
                 ("CASE B2 : J NOT EVALUATED (C-B+1)*" &
                  "(E-D+1)*(G-F+1)*(I-H+1) TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE B2 : Exception raised");
         end Case_B2;

         Case_B3 :
         declare
            B3 : array (2 .. 3, 1 .. 2) of T1 (1 .. 2, 2 .. 1);
         begin
            Cntr := (Choice_Index => 0);
            B3   :=
              (2 .. 3 =>
                 (1 .. 2 =>
                    (Calc (F, 1) .. Calc (G, 2) =>
                       (Calc (H, 2) .. Calc (I, 1) => Calc (J, 2)))));
            if Cntr (F) /= 4 then
               Failed ("CASE B3 : F NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (G) /= 4 then
               Failed ("CASE B3 : G NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (H) /= 4 then
               Failed ("CASE B3 : H NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (I) /= 4 then
               Failed ("CASE B3 : I NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (J) /= 0 then
               Failed ("CASE B3 : J NOT EVALUATED ZERO TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE B3 : Exception raised");
         end Case_B3;

         Case_B4 :
         declare
            B4 : array (2 .. 3, 1 .. 2) of T1 (2 .. 1, 1 .. 2);
         begin
            Cntr := (Choice_Index => 0);
            B4   :=
              (Calc (B, 2) .. Calc (C, 3) =>
                 (Calc (D, 1) .. Calc (E, 2) =>
                    (Calc (F, 2) .. Calc (G, 1) =>
                       (Calc (H, 1) .. Calc (I, 2) => Calc (J, 2)))));
            if Cntr (F) /= 4 then
               Failed ("CASE B4 : F NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (G) /= 4 then
               Failed ("CASE B4 : G NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (H) /= 4 then
               Failed ("CASE B4 : H NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (I) /= 4 then
               Failed ("CASE B4 : I NOT EVALUATED (C-B+1)*" & "(E-D+1) TIMES");
            end if;
            if Cntr (J) /= 0 then
               Failed ("CASE B4 : J NOT EVALUATED ZERO TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE B4 : Exception raised");
         end Case_B4;

      end Case_B;
   end;

   Result;

end C43208b;

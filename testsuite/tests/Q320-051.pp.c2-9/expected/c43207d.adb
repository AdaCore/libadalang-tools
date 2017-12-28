-- C43207D.ADA

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
-- FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM (F..G => (H..I => J)), CHECK
-- THAT:

--     D) J IS EVALUATED ONCE FOR EACH COMPONENT (ZERO TIMES IF THE
--        ARRAY IS NULL).

-- EG  01/18/84

with Report;

procedure C43207d is

   use Report;

begin

   Test
     ("C43207D",
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

      Case_D :
      begin

         Case_D1 :
         declare
            D1 : T0 (8 .. 4, 5 .. 1);
         begin
            Cntr := (Choice_Index => 0);
            D1   := (8 .. 4 => (5 .. 1 => Calc (J, 2)));
            if Cntr (J) /= 0 then
               Failed
                 ("CASE D1 : INCORRECT NUMBER " &
                  "OF EVALUATIONS. J EVALUATED" &
                  Integer'Image (Cntr (J)) &
                  " TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE D1 : EXCEPTION RAISED");
         end Case_D1;

         Case_D2 :
         declare
            D2 : T0 (8 .. 4, 5 .. 1);
         begin
            Cntr := (Choice_Index => 0);
            D2   :=
              (Calc (F, 8) .. Calc (G, 4) =>
                 (Calc (H, 5) .. Calc (I, 1) => Calc (J, 2)));
            if Cntr (J) /= 0 then
               Failed
                 ("CASE D2 : INCORRECT NUMBER " &
                  "OF EVALUATIONS. J EVALUATED" &
                  Integer'Image (Cntr (J)) &
                  " TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE D2 : EXCEPTION RAISED");
         end Case_D2;

         Case_D3 :
         declare
            D3 : T0 (3 .. 5, 1 .. 2);
         begin
            Cntr := (Choice_Index => 0);
            D3   := (3 .. 5 => (1 .. 2 => Calc (J, 2)));
            if Cntr (J) /= 6 then
               Failed
                 ("CASE D3 : INCORRECT NUMBER " &
                  "OF EVALUATIONS. J EVALUATED" &
                  Integer'Image (Cntr (J)) &
                  " TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE D3 : EXCEPTION RAISED");
         end Case_D3;

         Case_D4 :
         declare
            D4 : T0 (1 .. 2, 5 .. 7);
         begin
            Cntr := (Choice_Index => 0);
            D4   :=
              (Calc (F, 1) .. Calc (G, 2) =>
                 (Calc (H, 5) .. Calc (I, 7) => Calc (J, 2)));
            if Cntr (J) /= 6 then
               Failed
                 ("CASE D4 : INCORRECT NUMBER " &
                  "OF EVALUATIONS. J EVALUATED" &
                  Integer'Image (Cntr (J)) &
                  " TIMES");
            end if;
         exception
            when others =>
               Failed ("CASE D4 : EXCEPTION RAISED");
         end Case_D4;

      end Case_D;

   end;

   Result;

end C43207d;

-- C43212A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL SUBAGGREGATES FOR A PARTICULAR
-- DIMENSION DO NOT HAVE THE SAME BOUNDS.

-- EG  02/06/1984
-- JBG 3/30/84
-- JRK 4/18/86 CORRECTED ERROR TO ALLOW CONSTRAINT_ERROR TO BE
--               RAISED EARLIER.
-- EDS 7/15/98 AVOID OPTIMIZATION.

with Report;

procedure C43212a is

   use Report;

begin

   Test
     ("C43212A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL " &
      "SUBAGGREGATES FOR A PARTICULAR DIMENSION DO " &
      "NOT HAVE THE SAME BOUNDS");

   declare

      type Choice_Index is (H, I);
      type Choice_Cntr is array (Choice_Index) of Integer;

      Cntr : Choice_Cntr := (Choice_Index => 0);

      function Calc (A : Choice_Index; B : Integer) return Integer is
      begin
         Cntr (A) := Cntr (A) + 1;
         return Ident_Int (B);
      end Calc;

   begin

      Case_1 :
      declare

         type T is array (Integer range <>, Integer range <>) of Integer;

         A1 : T (1 .. 3, 2 .. 5) := (others => (others => 0));

      begin

         Cntr := (Choice_Index => 0);
         A1   :=
           (1 => (Calc (H, 2) .. Calc (I, 5) => -4),
            2 => (Calc (H, 3) .. Calc (I, 6) => -5),
            3 => (Calc (H, 2) .. Calc (I, 5) => -3));
         Failed
           ("CASE 1 : CONSTRAINT_ERROR NOT RAISED" &
            Integer'Image (A1 (1, 5)));

      exception

         when Constraint_Error =>
            if Cntr (H) < 2 and Cntr (I) < 2 then
               Failed
                 ("CASE 1 : BOUNDS OF SUBAGGREGATES " &
                  "NOT DETERMINED INDEPENDENTLY");
            end if;

         when others =>
            Failed ("CASE 1 : WRONG EXCEPTION RAISED");

      end Case_1;

      Case_1a :
      declare

         type T is array (Integer range <>, Integer range <>) of Integer;

         A1 : T (1 .. 3, 2 .. 3) := (1 .. 3 => (2 .. 3 => 1));

      begin

         if (1 .. 2 => (Ident_Int (3) .. Ident_Int (4) => 0), 3 => (1, 2)) = A1
         then
            begin
               Comment
                 (" IF SHOULD GENERATE CONSTRAINT_ERROR " &
                  Integer'Image (A1 (1, 2)));
            exception
               when others =>
                  Failed ("CASE 1A : CONSTRAINT_ERROR NOT RAISED");
            end;
         end if;
         Failed ("CASE 1A : CONSTRAINT_ERROR NOT RAISED");

      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("CASE 1A : WRONG EXCEPTION RAISED");

      end Case_1a;

      Case_2 :
      declare

         type T is array (Integer range <>, Integer range <>) of Integer;

         A2 : T (1 .. 3, Ident_Int (4) .. 2);

      begin

         Cntr := (Choice_Index => 0);
         A2   :=
           (1 => (Calc (H, 5) .. Calc (I, 3) => -4),
            3 => (Calc (H, 4) .. Calc (I, 2) => -5),
            2 => (Calc (H, 4) .. Calc (I, 2) => -3));
         Failed
           ("CASE 2 : CONSTRAINT_ERROR NOT RAISED " &
            Integer'Image (Ident_Int (A2'First (1))));
      exception

         when Constraint_Error =>
            if Cntr (H) < 2 and Cntr (I) < 2 then
               Failed
                 ("CASE 2 : BOUNDS OF SUBAGGREGATES " &
                  "NOT DETERMINED INDEPENDENTLY");
            end if;

         when others =>
            Failed ("CASE 2 : WRONG EXCEPTION RAISED");

      end Case_2;

   end;

   Result;

end C43212a;

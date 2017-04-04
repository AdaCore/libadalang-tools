-- C43103B.ADA

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
-- CHECK THAT IF A DISCRIMINANT DOES NOT GOVERN A VARIANT PART, ITS VALUE
-- CAN BE GIVEN BY A NONSTATIC EXPRESSION. ADDITIONAL CASES OF USE OF A
-- DISCRIMINANT THAT IS USED AS AN ARRAY INDEX BOUND.

-- PK  02/21/84
-- EG  05/30/84
-- EG  11/02/84
-- DN 12/01/95 REMOVED CONFORMANCE CHECKS WHERE RULES RELAXED. PWN 10/25/96
-- RESTORED CHECK WITH ADA 95 EXPECTED RESULTS INCLUDED.

with Report; use Report;

procedure C43103b is

   subtype Int is Integer range 1 .. 3;

   type A2 is array (Int range <>, Int range <>) of Integer;

   subtype Dint is Integer range 0 .. 10;

   type Rec (D, E : Dint := Ident_Int (1)) is record
      U : A2 (1 .. D, E .. 3) := (1 .. D => (E .. 3 => Ident_Int (1)));
   end record;

begin

   Test
     ("C43103B",
      "CHECK THAT IF A DISCRIMINANT DOES NOT GOVERN " &
      "A VARIANT PART, ITS VALUE CAN BE GIVEN BY A " &
      "NONSTATIC EXPRESSION");

-- SIMPLE DECLARATIONS

   begin

      declare

         L : Rec (Ident_Int (2), Ident_Int (2));
         K : Rec (Ident_Int (0), Ident_Int (1));
         M : Rec (Ident_Int (3), Ident_Int (4));

      begin
         if L.U'First (1) /= Ident_Int (1) or
           L.U'Last (1) /= Ident_Int (2) or
           L.U'First (2) /= Ident_Int (2) or
           L.U'Last (2) /= Ident_Int (3)
         then
            Failed ("1.1 - INCORRECT BOUNDS");
         end if;
         if K.U'First (1) /= Ident_Int (1) or
           K.U'Last (1) /= Ident_Int (0) or
           K.U'First (2) /= Ident_Int (1) or
           K.U'Last (2) /= Ident_Int (3)
         then
            Failed ("1.2 - INCORRECT BOUNDS");
         end if;
         if M.U'First (1) /= Ident_Int (1) or
           M.U'Last (1) /= Ident_Int (3) or
           M.U'First (2) /= Ident_Int (4) or
           M.U'Last (2) /= Ident_Int (3)
         then
            Failed ("1.3 - INCORRECT BOUNDS");
         end if;
         if M.U'Length (1) /= 3 or M.U'Length (2) /= 0 then
            Failed ("1.4 - INCORRECT ARRAY LENGTH");
         end if;
      end;

   exception

      when others =>
         Failed ("1.5 - EXCEPTION RAISED");

   end;

-- EXPLICIT INITIAL VALUE - OK

   begin

      declare
         O : constant Rec :=
           (Ident_Int (2),
            Ident_Int (2),
            ((1, Ident_Int (2)), (Ident_Int (2), 3)));
      begin
         if O.U'First (1) /= Ident_Int (1) or
           O.U'Last (1) /= Ident_Int (2) or
           O.U'First (2) /= Ident_Int (2) or
           O.U'Last (2) /= Ident_Int (3)
         then
            Failed ("2.1 - INCORRECT BOUNDS");
         end if;
      end;

   exception

      when others =>
         Failed ("2.2 - EXCEPTION RAISED");
   end;

-- EXPLICIT INITIAL VALUE: NULL ARRAY WITH WRONG BOUNDS

   begin

      declare
         P : constant Rec :=
           (Ident_Int (0),
            Ident_Int (2),
            (Ident_Int (3) .. Ident_Int (0) => (Ident_Int (2), 3)));
      begin
         null;
      end;

   exception

      when Constraint_Error =>
         Failed ("3.1 -  CONSTRAINT_ERROR RAISED");
      when others =>
         Failed ("3.2 -  WRONG EXCEPTION RAISED");
   end;

-- EXPLICIT INITIAL VALUE: NULL ARRAY WITH WRONG BOUNDS

   begin

      declare
         P : constant Rec :=
           (Ident_Int (0),
            Ident_Int (2),
            (Ident_Int (3) .. Ident_Int (0) => (others => Ident_Int (2))));
      begin
         null;
      end;

   exception

      when Constraint_Error =>
         Failed ("4.1 - CONSTRAINT_ERROR RAISED");
      when others =>
         Failed ("4.2 - WRONG EXCEPTION RAISED");

   end;

-- EXPLICIT INITIAL VALUE: NULL ARRAY WITH WRONG BOUNDS 2ND DIM.

   begin

      declare
         P : constant Rec :=
           (Ident_Int (0),
            Ident_Int (2),
            (Ident_Int (1) .. Ident_Int (0) =>
               (Ident_Int (1) .. Ident_Int (2) => 1)));
      begin
         null;
      end;

   exception

      when Constraint_Error =>
         Failed ("5.1 - CONSTRAINT_ERROR RAISED");
      when others =>
         Failed ("5.2 - WRONG EXCEPTION RAISED");

   end;

   Result;

end C43103b;

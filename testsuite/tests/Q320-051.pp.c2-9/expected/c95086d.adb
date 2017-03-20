-- C95086D.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR ACCESS PARAMETERS
--    BEFORE OR AFTER THE ENTRY CALL, WHEN AN UNCONSTRAINED ACTUAL
--    OUT ACCESS PARAMETER DESIGNATES AN OBJECT (PRIOR TO THE
--    ENTRY CALL) WITH CONSTRAINTS DIFFERENT FROM THE FORMAL
--    PARAMETER.
--
--   SUBTESTS ARE:
--       (A) STATIC LIMITED PRIVATE DISCRIMINANT.
--       (B) DYNAMIC ONE DIMENSIONAL BOUNDS.

-- RJW 2/3/86

with Report; use Report;
procedure C95086d is

begin
   Test
     ("C95086D",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "BEFORE AND AFTER THE ENTRY CALL, WHEN AN UNCONSTRAINED " &
      "ACTUAL OUT ACCESS PARAMETER DESIGNATES AN OBJECT (PRIOR " &
      "TO THE ENTRY CALL) WITH CONSTRAINTS DIFFERENT FROM THE " &
      "FORMAL PARAMETER");

   --------------------------------------------------

   declare -- (A)

      package Pkg is
         subtype Int is Integer range 0 .. 5;
         type T (I : Int := 0) is limited private;
      private
         type Arr is array (Integer range <>) of Integer;
         type T (I : Int := 0) is record
            J : Integer;
            A : Arr (1 .. I);
         end record;
      end Pkg;

      use Pkg;

      type A is access T;
      subtype Sa is A (3);
      V      : A       := new T (2);
      Called : Boolean := False;

      task T1 is
         entry P (X : out Sa);
      end T1;

      task body T1 is
      begin
         accept P (X : out Sa) do
            Called := True;
            X      := new T (3);
         end P;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (A)");
      end T1;

   begin -- (A)

      T1.P (V);

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE ENTRY CALL - (A)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (A)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (A)");
   end; -- (A)

   --------------------------------------------------

   declare -- (B)

      type A is access String;
      subtype Sa is A (1 .. 2);
      V      : A       := new String (Ident_Int (5) .. Ident_Int (7));
      Called : Boolean := False;

      task T1 is
         entry P (X : out Sa);
      end T1;

      task body T1 is
      begin
         accept P (X : out Sa) do
            Called := True;
            X      := new String (Ident_Int (1) .. Ident_Int (2));
         end P;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN TASK - (B)");
      end T1;

   begin -- (B)

      T1.P (V);

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE ENTRY CALL - (B)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (B)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (B)");
   end; -- (B)

   --------------------------------------------------

   Result;
end C95086d;

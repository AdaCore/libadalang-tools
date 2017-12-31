-- C64105D.ADA

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
--   IN THE FOLLOWING CIRCUMSTANCES:
--       (1)
--       (2)
--       (3) BEFORE OR AFTER THE CALL, WHEN AN UNCONSTRAINED ACTUAL
--           OUT ACCESS PARAMETER DESIGNATES AN OBJECT (PRIOR TO THE
--           CALL) WITH CONSTRAINTS DIFFERENT FROM THE FORMAL
--           PARAMETER.
--   SUBTESTS ARE:
--       (G) CASE 3, STATIC LIMITED PRIVATE DISCRIMINANT.
--       (H) CASE 3, DYNAMIC ONE DIMENSIONAL BOUNDS.

-- JRK 3/20/81
-- SPS 10/26/82

with Report;
procedure C64105d is

   use Report;

begin
   Test
     ("C64105D",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "BEFORE AND AFTER THE CALL, WHEN AN UNCONSTRAINED ACTUAL " &
      "OUT ACCESS PARAMETER DESIGNATES AN OBJECT (PRIOR TO THE " &
      "CALL) WITH CONSTRAINTS DIFFERENT FROM THE FORMAL " & "PARAMETER");

   --------------------------------------------------

   declare -- (G)

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

      procedure P (X : out Sa) is
      begin
         Called := True;
         X      := new T (3);
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (G)");
      end P;

   begin -- (G)

      P (V);

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (G)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (G)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (G)");
   end; -- (G)

   --------------------------------------------------

   declare -- (H)

      type A is access String;
      subtype Sa is A (1 .. 2);
      V      : A       := new String (Ident_Int (5) .. Ident_Int (7));
      Called : Boolean := False;

      procedure P (X : out Sa) is
      begin
         Called := True;
         X      := new String (Ident_Int (1) .. Ident_Int (2));
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (H)");
      end P;

   begin -- (H)

      P (V);

   exception
      when Constraint_Error =>
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL - (H)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (H)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (H)");
   end; -- (H)

   --------------------------------------------------

   Result;
end C64105d;

-- C61008A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF THE DEFAULT VALUE FOR A FORMAL
-- PARAMETER DOES NOT SATISFY THE CONSTRAINTS OF THE SUBTYPE_INDICATION WHEN
-- THE DECLARATION IS ELABORATED, ONLY WHEN THE DEFAULT IS USED.

--   SUBTESTS ARE:
--        (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--            INITIALIZED WITH A STATIC AGGREGATE.
--        (B) A SCALAR PARAMETER WITH NON-STATIC RANGE CONSTRAINTS
--            INITIALIZED WITH A STATIC VALUE.
--        (C) A RECORD PARAMETER WHOSE COMPONENTS HAVE NON-STATIC
--            CONSTRAINTS INITIALIZED WITH A STATIC AGGREGATE.
--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.
--        (E) A RECORD PARAMETER WITH A NON-STATIC CONSTRAINT
--            INITIALIZED WITH A STATIC AGGREGATE.

-- DAS  1/20/81
-- SPS 10/26/82
-- VKG 1/13/83
-- SPS 2/9/83
-- BHS 7/9/84

with Report;
procedure C61008a is

   use Report;

begin

   Test
     ("C61008A",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF " &
      "AN INITIALIZATION VALUE DOES NOT SATISFY " &
      "CONSTRAINTS ON A FORMAL PARAMETER");

   --------------------------------------------------

   declare -- (A)

      procedure Pa (I1, I2 : Integer) is

         type A1 is array (1 .. I1, 1 .. I2) of Integer;

         procedure Pa1 (A : A1 := ((1, 0), (0, 1))) is
         begin
            Failed ("BODY OF PA1 EXECUTED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN PA1");
         end Pa1;

      begin
         Pa1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PA1");
      end Pa;

   begin   -- (A)
      Pa (Ident_Int (1), Ident_Int (10));
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN CALL TO PA");
   end;    -- (A)

   --------------------------------------------------

   declare -- (B)

      procedure Pb (I1, I2 : Integer) is

         subtype Int is Integer range I1 .. I2;

         procedure Pb1 (I : Int := -1) is
         begin
            Failed ("BODY OF PB1 EXECUTED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN PB1");
         end Pb1;

      begin
         Pb1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PB1");
      end Pb;

   begin   -- (B)
      Pb (Ident_Int (0), Ident_Int (63));
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN CALL TO PB");
   end;    -- (B)

   --------------------------------------------------

   declare -- (C)

      procedure Pc (I1, I2 : Integer) is
         type Ar1 is array (1 .. 3) of Integer range I1 .. I2;
         type Rec is record
            I : Integer range I1 .. I2;
            A : Ar1;
         end record;

         procedure Pc1 (R : Rec := (-3, (0, 2, 3))) is
         begin
            Failed ("BODY OF PC1 EXECUTED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN PC1");
         end Pc1;

      begin
         Pc1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PC1");
      end Pc;

   begin   -- (C)
      Pc (Ident_Int (1), Ident_Int (3));
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN CALL TO PC");
   end;    -- (C)

   --------------------------------------------------

   declare -- (D1)

      procedure P1d (I1, I2 : Integer) is

         type A1 is array (1 .. 2, 1 .. 2) of Integer range I1 .. I2;

         procedure P1d1 (A : A1 := ((1, -1), (1, 2))) is
         begin
            Failed ("BODY OF P1D1 EXECUTED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN P1D1");
         end P1d1;

      begin
         P1d1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - P1D1");
      end P1d;

   begin   -- (D1)
      P1d (Ident_Int (1), Ident_Int (2));
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN CALL TO P1D");
   end;    -- (D1)

   --------------------------------------------------

   declare -- (D2)

      procedure P2d (I1, I2 : Integer) is

         type A1 is array (1 .. 2, 1 .. 2) of Integer range I1 .. I2;

         procedure P2d1 (A : A1 := (3 .. 4 => (1, 2))) is
         begin
            Failed ("BODY OF P2D1 EXECUTED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN P2D1");
         end P2d1;

      begin
         P2d1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - P2D1");
      end P2d;

   begin  -- (D2)
      P2d (Ident_Int (1), Ident_Int (2));
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN CALL TO P2D");
   end;   -- (D2)

   --------------------------------------------------

   declare -- (E)

      procedure Pe (I1, I2 : Integer) is
         subtype Int is Integer range 0 .. 10;
         type Arr is array (1 .. 3) of Int;
         type Rec (I : Int) is record
            A : Arr;
         end record;

         subtype Rec4 is Rec (I1);

         procedure Pe1 (R : Rec4 := (3, (1, 2, 3))) is
         begin
            Failed ("BODY OF PE1 EXECUTED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN PE1");
         end Pe1;

      begin
         Pe1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PE1");
      end Pe;

   begin   -- (E)
      Pe (Ident_Int (4), Ident_Int (10));
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN CALL TO PE");
   end;    -- (E)

   --------------------------------------------------

   Result;

end C61008a;

-- C64103C.ADA

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
-- CHECK THAT THE APPROPRIATE EXCEPTION IS RAISED FOR TYPE CONVERSIONS ON IN
-- OUT ARRAY PARAMETERS. IN PARTICULAR:
--   (A) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL WHEN THE ACTUAL
--       COMPONENT'S CONSTRAINTS DIFFER FROM THE FORMAL COMPONENT'S
--       CONSTRAINTS.
--   (B) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL WHEN CONVERSION TO
--       AN UNCONSTRAINED ARRAY TYPE CAUSES AN ACTUAL INDEX BOUND TO LIE
--       OUTSIDE OF A FORMAL INDEX SUBTYPE FOR A NON-NULL DIMENSION (SEE
--       AI-00313 FOR MULTIDIMENSIONAL CASE)
--   (C) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL FOR CONVERSION TO A
--       CONSTRAINED ARRAY TYPE WHEN THE NUMBER OF COMPONENTS PER
--       DIMENSION OF THE ACTUAL DIFFERS FROM THAT OF THE FORMAL.
--   (D) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL WHEN CONVERSION TO AN
--       UNCONSTRAINED ARRAY TYPE CAUSES AN ACTUAL INDEX BOUND TO LIE
--       OUTSIDE OF THE BASE INDEX TYPE OF THE FORMAL.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- CPP 07/19/84
-- JBG 06/05/85
-- EG 10/29/85 FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- MRM 03/30/93 REMOVE NUMERIC_ERROR FOR 9X COMPATIBILITY PWN 01/31/95 REMOVED
-- INCONSISTENCIES WITH ADA 9X.

with System;
with Report; use Report;
procedure C64103c is

begin
   Test
     ("C64103C",
      "CHECK THAT APPROPRIATE EXCEPTION IS RAISED ON " &
      "TYPE CONVERSIONS OF IN OUT ARRAY PARAMETERS");

   -----------------------------------------------

   declare   -- (A)
   begin     -- (A)

      declare
         type Subint is range 0 .. 8;
         type Array_Type is array (Subint range <>) of Boolean;
         A0 : Array_Type (0 .. 3) := (0 .. 3 => True);

         procedure P2 (X : in out Array_Type) is
         begin
            null;
         end P2;
      begin
         P2 (Array_Type (A0));                  -- OK.
      exception
         when others =>
            Failed ("EXCEPTION RAISED -P2 (A)");
      end;

   end; -- (A)

   -----------------------------------------------

   declare   -- (B1) NON-NULL ACTUAL PARAMETER

      type Subint is range 0 .. 8;
      type Array_Type is array (Subint range <>) of Boolean;
      type Ar1 is array (Integer range <>) of Boolean;
      A1 : Ar1 (-1 .. 7) := (-1 .. 7 => True);
      A2 : Ar1 (1 .. 9)  := (1 .. 9 => True);

      procedure P1 (X : in out Array_Type) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P1 (B)");
      end P1;

   begin     -- (B1)

      begin
         Comment ("CALL TO P1 (B1) ON A1");
         P1 (Array_Type (A1));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B1)");
      end;

      begin
         Comment ("CALL TO P1 (B1) ON A2");
         P1 (Array_Type (A2));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B1)");
      end;

   end; -- (B1)

   declare   -- (B2) NULL ACTUAL PARAMETER; MULTIDIMENSIONAL

      type Subint is range 0 .. 8;
      type Array_Type is array (Subint range <>, Subint range <>) of Boolean;
      type Ar1 is array (Integer range <>, Integer range <>) of Boolean;
      A1 : Ar1 (Ident_Int (-1) .. 7, 5 .. 4) := (others => (others => True));
      A2 : Ar1 (5 .. 4, 1 .. Ident_Int (9))  := (others => (others => True));
      procedure P1 (X : in out Array_Type) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P1 (B)");
      end P1;

   begin     -- (B2)

      begin
         Comment ("CALL TO P1 (B2) ON A1");
         P1 (Array_Type (A1));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B2)");
      end;

      begin
         Comment ("CALL TO P1 (B2) ON A2");
         P1 (Array_Type (A2));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B2)");
      end;

   end; -- (B2)

   -----------------------------------------------

   begin     -- (C)

      declare
         type Index1 is range 1 .. 3;
         type Index2 is range 1 .. 4;
         type Ar_Type is array (Index1, Index2) of Boolean;
         A0 : Ar_Type := (1 .. 3 => (1 .. 4 => False));

         type I1 is range 1 .. 4;
         type I2 is range 1 .. 3;
         type Array_Type is array (I1, I2) of Boolean;

         procedure P1 (X : in out Array_Type) is
         begin
            Failed ("EXCEPTION NOT RAISED BEFORE CALL -P1 (C)");
         end P1;
      begin
         P1 (Array_Type (A0));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (C)");
      end;

   end; -- (C)

   -----------------------------------------------

   declare   -- (D)
   begin     -- (D)

      declare
         type Sm_Int is range 0 .. 2;
         type Lg is range 0 .. System.Max_Int;
         subtype Lg_Int is Lg range System.Max_Int - 3 .. System.Max_Int;
         type Ar_Small is array (Sm_Int range <>) of Boolean;
         type Ar_Large is array (Lg_Int range <>) of Boolean;
         A0 : Ar_Large (System.Max_Int - 2 .. System.Max_Int) :=
           (System.Max_Int - 2 .. System.Max_Int => True);

         procedure P1 (X : in out Ar_Small) is
         begin
            Failed ("EXCEPTION NOT RAISED BEFORE CALL -P1 (D)");
         end P1;
      begin
         if Lg (Sm_Int'Base'Last) < Lg_Int'Base'Last then
            P1 (Ar_Small (A0));
         else
            Comment ("NOT APPLICABLE -P1 (D)");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - P1 (D)");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - P1 (D)");
      end;

   end; -- (D)

   -----------------------------------------------

   Result;

end C64103c;

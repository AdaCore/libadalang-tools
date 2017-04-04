-- C35904B.ADA

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
-- CHECK THAT INCOMPATIBLE FIXED POINT CONSTRAINTS RAISE
-- CONSTRAINT_ERROR FOR GENERIC FORMAL TYPES.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- RJW 6/20/86
-- DWC 07/24/87     -- ADDED NUMERIC_ERROR HANDLERS.
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.
-- EDS 07/16/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C35904b is

   generic
      type Fix is delta <>;
   procedure Proc (Str : String);

   procedure Proc (Str : String) is
      subtype Sfix is Fix delta 0.1 range -1.0 .. 1.0;
      -- DEFINE AN OBJECT OF SUBTYPE SFIX AND USE IT TO AVOID
      -- OPTIMIZATION OF SUBTYPE
      Sfix_Var : Sfix := Sfix (Ident_Int (0));
   begin
      Failed
        ("NO EXCEPTION RAISED FOR " &
         Str &
         " " &
         Sfix'Image (Sfix_Var));  --USE SFIX_VAR
   end Proc;

begin

   Test
     ("C35904B",
      "CHECK THAT INCOMPATIBLE FIXED POINT " &
      "CONSTRAINTS RAISE CONSTRAINT_ERROR " &
      "FOR GENERIC FORMAL TYPES");

-- TEST FOR INCORRECT SUBTYPE DEFINITION ON ACCURACY BETWEEN TYPE AND
-- SUBTYPE DEFINITIONS.

   begin

      declare

         type Fix1 is delta 0.5          -- DELTA IS SMALLER FOR
         range -2.0 .. 2.0;    -- SUBTYPE THEN FOR
         -- TYPE.

         procedure Nproc is new Proc (Fix1);

      begin
         Nproc ("INCOMPATIBLE DELTA");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("INCORRECT EXCEPTION RAISED WHILE CHECKING " & "DELTA CONSTRAINT");
   end;

-- TEST THAT CONSTRAINT_ERROR IS RAISED
-- FOR A RANGE VIOLATION.

   begin

      declare

         type Fix2 is delta 0.1 range 0.0 .. 2.0; -- LOWER
         -- BOUND.

         procedure Nproc is new Proc (Fix2);

      begin
         Nproc ("FIXED POINT LOWER BOUND CONSTRAINT VIOLATION");
      end;

   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR " & "LOWER BOUND VIOLATION");
      when others =>
         Failed
           ("INCORRECT EXCEPTION RAISED WHILE CHECKING " &
            "FIXED POINT LOWER BOUND CONSTRAINT");
   end;

-- TEST THAT CONSTRAINT_ERROR IS RAISED
-- FOR A RANGE VIOLATION.

   begin

      declare

         type Fix3 is delta 0.1 range -2.0 .. 0.0;  -- UPPER
         -- BOUND.

         procedure Nproc is new Proc (Fix3);
      begin
         Nproc ("FIXED POINT UPPER BOUND CONSTRAINT VIOLATION");
      end;

   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR " & "UPPER BOUND VIOLATION");
      when others =>
         Failed
           ("INCORRECT EXCEPTION RAISED WHILE CHECKING " &
            "FIXED POINT UPPER BOUND CONSTRAINT");
   end;

   Result;

end C35904b;

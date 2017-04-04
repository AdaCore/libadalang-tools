-- C55B15A.ADA

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
-- CHECK THAT IF A DISCRETE_RANGE OF THE FORM 'ST RANGE L..R'
--    RAISES AN EXCEPTION BECAUSE  L  OR  R  IS A NON-STATIC
--    EXPRESSION WHOSE VALUE IS OUTSIDE  THE RANGE OF VALUES
--    ASSOCIATED WITH  ST  (OR BECAUSE  ST'FIRST  IS NON-STATIC
--    AND  L  IS STATIC AND LESS THAN  ST'FIRST ; SIMILARLY FOR
--     ST'LAST  AND  R ), CONTROL DOES NOT ENTER THE LOOP.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- RM  04/13/81
-- SPS 11/01/82
-- BHS 07/13/84
-- EG 10/28/85 FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- MRM 03/30/93 REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY GJD 11/15/95 REMOVED
-- CASE OF POTENTIALLY STATICALLY INCOMPATIBLE RANGE.

with System;
with Report;
procedure C55b15a is

   use Report;

begin

   Test
     ("C55B15A",
      "WHEN  'FOR  I  IN  ST RANGE L..R  LOOP' " &
      "RAISES AN EXCEPTION, CONTROL DOES NOT ENTER " &
      "THE BODY OF THE LOOP");

   -------------------------------------------------------------------
   ----------------- STATIC (SUB)TYPE, DYNAMIC RANGE -----------------

   declare

      subtype St is Integer range 1 .. 4;

      First  : constant Integer := Ident_Int (1);
      Second : constant Integer := Ident_Int (2);
      Third  : constant Integer := Ident_Int (3);
      Fourth : constant Integer := Ident_Int (4);
      Fifth  : constant Integer := Ident_Int (5);
      Tenth  : constant Integer := Ident_Int (10);
      Zeroth : constant Integer := Ident_Int (0);

   begin

      begin

         for I in St range 3 .. Tenth loop
            Failed ("EXCEPTION NOT RAISED (I1)");
         end loop;

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (I1)");

      end;

      begin

         for I in St range 0 .. Third loop
            Failed ("EXCEPTION NOT RAISED (I2)");
         end loop;

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (I2)");

      end;
   end;

   -------------------------------------------------------------------
   ----------------- DYNAMIC (SUB)TYPE, STATIC RANGE -----------------

   declare

      type Enum is (Aminus, A, B, C, D, E, F, G, H, I, J);

      subtype St is
        Enum range Enum'Val (Ident_Int (1)) .. Enum'Val (Ident_Int (4));

      First  : constant Enum := A;
      Second : constant Enum := B;
      Third  : constant Enum := C;
      Fourth : constant Enum := D;
      Fifth  : constant Enum := E;
      Tenth  : constant Enum := J;
      Zeroth : constant Enum := Aminus;

   begin

      begin

         for I in St range C .. Tenth loop
            Failed ("EXCEPTION NOT RAISED (E1)");
         end loop;

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (E1)");

      end;

      begin

         for I in St range Aminus .. Third loop
            Failed ("EXCEPTION NOT RAISED (E2)");
         end loop;

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (E2)");

      end;

   end;

   declare

      subtype St is Character range Ident_Char ('A') .. Ident_Char ('D');

      First  : constant Character := 'A';
      Second : constant Character := 'B';
      Third  : constant Character := 'C';
      Fourth : constant Character := 'D';
      Fifth  : constant Character := 'E';
      Tenth  : constant Character := 'J';
      Zeroth : constant Character := '0';--ZERO; PRECEDES LETTERS

   begin

      begin

         for I in St range 'C' .. Tenth loop
            Failed ("EXCEPTION NOT RAISED (C1)");
         end loop;

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (C1)");

      end;

      begin

         for I in St range '0' .. Third loop -- ZERO..'C'
            Failed ("EXCEPTION NOT RAISED (C2)");
         end loop;

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED (C2)");

      end;

   end;

   Result;

end C55b15a;

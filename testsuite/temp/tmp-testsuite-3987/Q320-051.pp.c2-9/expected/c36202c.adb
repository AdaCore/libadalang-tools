-- C36202C.ADA

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
-- CHECK THAT 'LENGTH DOES NOT RAISE AN EXCEPTION
-- WHEN APPLIED TO A NULL ARRAY A, EVEN IF A'LAST - A'FIRST
-- WOULD RAISE CONSTRAINT_ERROR.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- L.BROWN  07/29/86
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

with Report; use Report;
with System; use System;

procedure C36202c is

   type Lrg_Int is range Min_Int .. Max_Int;

begin
   Test
     ("C36202C",
      "NO EXCEPTION IS RAISED FOR 'LENGTH " & "WHEN APPLIED TO A NULL ARRAY");

   declare
      type Lrg_Arr is array (Lrg_Int range Max_Int .. Min_Int) of Integer;
      Lrg_Obj : Lrg_Arr;

   begin
      if Lrg_Obj'Length /= 0 then
         Failed
           ("INCORRECT VALUE RETURNED BY 'LENGTH " & "FOR ONE-DIM NULL ARRAY");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR WAS RAISED " & "FOR ONE-DIM NULL ARRAY");
      when others =>
         Failed ("EXCEPTION RAISED FOR ONE-DIM " & "NULL ARRAY");
   end;

   declare
      type Lrg2_Arr is
        array
          (Lrg_Int range 1 .. 3, Lrg_Int range Max_Int .. Min_Int) of Integer;
   begin
      if Lrg2_Arr'Length (2) /= 0 then
         Failed
           ("INCORRECT VALUE RETURNED BY 'LENGTH " & "FOR TWO-DIM NULL ARRAY");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR WAS RAISED " & "FOR TWO-DIM NULL ARRAY");
      when others =>
         Failed ("EXCEPTION RAISED FOR TWO-DIM " & "NULL ARRAY");
   end;

   Result;

end C36202c;

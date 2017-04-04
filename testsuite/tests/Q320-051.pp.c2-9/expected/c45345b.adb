-- C45345B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF THE RESULT OF
--     CATENATION HAS PRECISELY THE MAXIMUM LENGTH PERMITTED BY THE
--     INDEX SUBTYPE.

-- RM  2/26/82

with Report; use Report;
procedure C45345b is

begin

   Test
     ("C45345B",
      "CHECK THAT  CONSTRAINT_ERROR  IS NOT RAISED" &
      " IF THE RESULT OF CATENATION HAS PRECISELY" &
      " THE MAXIMUM LENGTH PERMITTED BY THE" &
      " INDEX SUBTYPE");

   -------------------------------------------------------------------
   -----------------  STRG_VAR := STRG_LIT & STRG_LIT  ---------------

   declare

      X : String (1 .. 5);

   begin

      X := "ABCD" & "E";

   exception

      when Constraint_Error =>
         Failed ("'STRING & STRING' RAISED  CONSTRAINT_ERROR ");

      when others =>
         Failed ("'STRING & STRING' RAISED ANOTHER EXCEPTION");

   end;

   -------------------------------------------------------------------
   -----------------  STRG_VAR := STRG_LIT & CHARACTER  --------------

   declare

      X : String (1 .. 5);

   begin

      X := "ABCD" & 'E';

   exception

      when Constraint_Error =>
         Failed ("'STRING & STRING' RAISED  CONSTRAINT_ERROR ");

      when others =>
         Failed ("'STRING & STRING' RAISED ANOTHER EXCEPTION");

   end;

   -------------------------------------------------------------------
   -----------------  STRG_VAR := STRG_VAR & STRG_VAR  ---------------

   declare

      X : String (1 .. 5);
      A : constant String := "A";
      B : String (1 .. 4) := Ident_Str ("BCDE");

   begin

      X := A & B;

   exception

      when Constraint_Error =>
         Failed ("'STRING & STRING' RAISED  CONSTRAINT_ERROR ");

      when others =>
         Failed ("'STRING & STRING' RAISED ANOTHER EXCEPTION");

   end;

   -------------------------------------------------------------------

   Result;

end C45345b;

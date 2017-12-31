-- C45534B.ADA

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
-- OBJECTIVE:
--     CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN A
--     FIXED POINT VALUE IS DIVIDED BY ZERO (EITHER AN INTEGER ZERO OR
--     A FIXED POINT ZERO).

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- HISTORY:
--     BCB 07/14/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC ERROR FOR 9X CONSISTENCY

with Report; use Report;

procedure C45534b is

   type Fix is delta 2.0**(-1) range -2.0 .. 2.0;
   type Fix2 is delta 2.0**(-1) range -3.0 .. 3.0;

   A     : Fix  := 1.0;
   B     : Fix;
   Zero  : Fix  := 0.0;
   Zero2 : Fix2 := 0.0;

   function Ident_Flt (One, Two : Fix) return Boolean is
   begin
      return One = Fix (Two * Fix (Ident_Int (1)));
   end Ident_Flt;

begin
   Test
     ("C45534B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "A FIXED POINT VALUE IS " &
      "DIVIDED BY ZERO (EITHER AN INTEGER ZERO OR A " & "FIXED POINT ZERO)");

   begin
      B := A / Ident_Int (0);
      Failed ("NO EXCEPTION RAISED FOR DIVISION BY INTEGER ZERO");
      if Ident_Flt (B, B) then
         Comment ("DON'T OPTIMIZE B");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED");
   end;

   begin
      B := Fix (A / Zero);
      Failed ("NO EXCEPTION RAISED FOR DIVISION BY FIXED POINT " & "ZERO - 1");
      if Ident_Flt (B, B) then
         Comment ("DON'T OPTIMIZE B");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED");
   end;

   begin
      B := Fix (A / Zero2);
      Failed ("NO EXCEPTION RAISED FOR DIVISION BY FIXED POINT " & "ZERO - 2");
      if Ident_Flt (B, B) then
         Comment ("DON'T OPTIMIZE B");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED");
   end;

   Result;
end C45534b;

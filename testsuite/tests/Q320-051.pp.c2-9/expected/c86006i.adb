-- C86006I.ADA

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
--     CHECK THAT THE IDENTIFIERS "BOOLEAN, TRUE, AND FALSE" AND THE
--     IDENTIFIERS "INTEGER, NATURAL, AND POSITIVE" ARE DECLARED IN
--     THE PACKAGE "STANDARD", ALONG WITH THE OPERATORS OF THE TYPE
--     BOOLEAN AND THE TYPE INTEGER.

-- HISTORY:
--     DTN 04/15/92 CONSOLIDATION OF C86006A AND C86006B.

with Report; use Report;
procedure C86006i is

   Abool, Bbool : Standard.Boolean  := Standard.False;
   Cbool        : Standard.Boolean  := Standard.True;
   Int1         : Standard.Integer  := -2;
   Nat1         : Standard.Natural  := 0;
   Pos1, Pos2   : Standard.Positive := 2;

begin

   Test
     ("C86006I",
      "CHECK THAT THE IDENTIFIERS ""BOOLEAN, TRUE, AND " &
      "FALSE"" AND THE IDENTIFIERS ""INTEGER, NATURAL, " &
      "AND POSITIVE"" ARE DECLARED IN THE PACKAGE " &
      """STANDARD"", ALONG WITH THE OPERATORS OF THE " &
      "TYPE BOOLEAN AND THE TYPE INTEGER");

   -- STANDARD.">" OPERATOR.

   if Standard.">" (Abool, Bbool) then
      Failed ("STANDARD.> FAILED FOR BOOLEAN TYPE");
   end if;

   if Standard.">" (Int1, Nat1) then
      Failed ("STANDARD.> FAILED FOR INTEGER-NATURAL TYPE");
   end if;

   -- STANDARD."/=" OPERATOR.

   if Standard."/=" (Abool, Bbool) then
      Failed ("STANDARD./= FAILED FOR BOOLEAN TYPE");
   end if;

   if Standard."/=" (Pos1, Pos2) then
      Failed ("STANDARD./= FAILED FOR INTEGER-POSITIVE TYPE");
   end if;

   -- STANDARD."AND" OPERATOR.

   if Standard."AND" (Cbool, Abool) then
      Failed ("STANDARD.AND FAILED");
   end if;

   -- STANDARD."-" BINARY OPERATOR.

   if Standard."-" (Int1, Pos1) /= Ident_Int (-4) then
      Failed ("STANDARD.- FAILED");
   end if;

   -- STANDARD."-" UNARY OPERATOR.

   if Standard."-" (Int1) /= Ident_Int (2) then
      Failed ("STANDARD.UNARY - FAILED");
   end if;

   -- STANDARD."REM" OPERATOR.

   if Standard."REM" (Ident_Int (14), Ident_Int (5)) /= Ident_Int (4) then
      Failed ("STANDARD.REM (++=+) FAILED");
   end if;

   -- STANDARD."MOD" OPERATOR.

   if Standard."MOD" (Ident_Int (14), Ident_Int (-5)) /= Ident_Int (-1) then
      Failed ("STANDARD.MOD (+-=-) FAILED");
   end if;

   Result;

end C86006i;

-- C87B04B.ADA

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
--      CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:

--      IN AN ACCURACY CONSTRAINT OF A SUBTYPE INDICATION, THE
--      EXPRESSIONS FOR THE LOWER AND UPPER BOUNDS MUST BE COMPATIBLE
--      WITH THE SUBTYPE'S EXPLICIT TYPEMARK.

-- HISTORY:
--      TRH 06/29/82  CREATED ORIGINAL TEST.
--      BCB 11/12/87  CHANGED HEADER TO STANDARD FORMAT.  CORRECTED
--                    CONSTRAINT ERRORS.
--      KAS 11/24/95  DELETED SUBTYPE DIGITS CONSTRAINT

with Report; use Report;

procedure C87b04b is

   type Exact is digits 5 range -1.0 .. 1.0;
   type Hex is delta 2.0**(-4) range -1.0 .. 1.0;

   function F1 return Exact is
   begin
      return 0.0;
   end F1;

   function F1 return Float is
   begin
      Failed
        ("RESOLUTION INCORRECT - ACCURACY CONSTRAINT OF " &
         "SUBTYPE INDICATION - F1");
      return 0.0;
   end F1;

   function "+" (X : Integer) return Hex is
   begin
      return 0.0;
   end "+";

   function "+" (X : Integer) return Float is
   begin
      Failed
        ("RESOLUTION INCORRECT - ACCURACY CONSTRAINT OF " &
         "SUBTYPE INDICATION - +");
      return 0.0;
   end "+";

begin
   Test
     ("C87B04B",
      "OVERLOADED EXPRESSIONS IN ACCURACY CONTRAINTS" &
      " OF FLOATING/FIXED POINT SUBTYPE INDICATIONS");

   declare
      subtype Close is Exact range -1.0 .. F1;
      subtype Bin is Hex delta 2.0**(-1) range "+" (0) .. 0.5;

   begin
      null;
   end;

   Result;
end C87b04b;

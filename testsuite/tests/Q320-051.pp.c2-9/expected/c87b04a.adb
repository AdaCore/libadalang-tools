-- C87B04A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- IN A RANGE CONSTRAINT OF A SUBTYPE INDICATION, THE EXPRESSIONS FOR THE LOWER
-- AND UPPER BOUNDS MUST BE COMPATIBLE WITH THE SUBTYPE'S EXPLICIT TYPEMARK.

-- TRH  28 JUNE 82
-- JBG 3/8/84

with Report; use Report;
procedure C87b04a is

   type Age is new Integer range 1 .. 120;
   type Base10 is new Integer range 0 .. 9;

   function F1 return Age is
   begin
      return 18;
   end F1;

   function F1 return Integer is
   begin
      Failed
        ("RESOLUTION INCORRECT - RANGE CONSTRAINT OF " & "SUBTYPE INDICATION");
      return 0;
   end F1;

   function "+" (X : Integer) return Base10 is
   begin
      return 1;
   end "+";

   function "+" (X : Integer) return Integer is
   begin
      Failed
        ("RESOLUTION INCORRECT - RANGE CONSTRAINT OF " & "SUBTYPE INDICATION");
      return -X;
   end "+";

begin
   Test
     ("C87B04A",
      "OVERLOADED EXPRESSIONS IN RANGE CONTRAINTS" &
      " OF SUBTYPE INDICATIONS");

   declare
      subtype Minor is Age range 1 .. F1;

   begin
      for I in Base10 range +(Integer'(0)) .. 0 loop
         Failed
           ("RESOLUTION INCORRECT - SUBTYPE INDICATION " &
            " IN LOOP CONSTRUCT");
      end loop;
   end;

   Result;
end C87b04a;

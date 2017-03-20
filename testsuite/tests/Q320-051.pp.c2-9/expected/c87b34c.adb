-- C87B34C.ADA

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

-- FOR A MEMBERSHIP RELATION WITH A TYPEMARK, THE TYPE OF THE
-- SIMPLE EXPRESSION MUST BE THE BASE TYPE OF THE TYPEMARK.

-- TRH  15 SEPT 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b34c is

   type Vowel is (A, E, I, O, U, Vocalic_Y);
   type Alpha is (A, 'A');
   type Grade is (A, B, C, D, F);
   subtype Bad_Grade is Grade range D .. F;
   subtype Passing is Grade range A .. C;

   generic
      type T is private;
      Arg : in T;
   function F1 return T;

   function F1 return T is
   begin
      Failed
        ("RESOLUTION INCORRECT - EXPRESSION IN MEMBER" &
         "SHIP TEST WITH TYPEMARK MUST MATCH TYPEMARK");
      return Arg;
   end F1;

   function F is new F1 (Character, 'A');
   function F is new F1 (Duration, 1.0);
   function F is new F1 (Integer, -10);
   function F is new F1 (Boolean, True);
   function F is new F1 (Float, 1.0);
   function F is new F1 (Vowel, A);
   function F is new F1 (Alpha, A);

begin
   Test
     ("C87B34C",
      "OVERLOADED EXPRESSION IN MEMBERSHIP TEST " & "WITH A TYPEMARK");

   if (F not in Grade) or (F not in Bad_Grade) or (F in Passing) then
      Failed ("RESOLUTION INCORRECT FOR MEMBERSHIP TEST " & "WITH TYPEMARK");
   end if;

   Result;

end C87b34c;

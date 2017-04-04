-- C87B07C.ADA

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
-- FOR THE ATTRIBUTE OF THE FORM T'VALUE (X), THE OPERAND X MUST
-- BE OF THE PREDEFINED TYPE STRING. THE RESULT IS OF TYPE T.

-- TRH  13 SEPT 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b07c is

   type Char is new Character;
   type Lits is (' ', '+', '1');
   type Word is array (Positive range 1 .. 4) of Character;
   type Line is array (Positive range 1 .. 4) of Char;
   type List is array (Positive range 1 .. 4) of Lits;
   type Str is array (Positive range 1 .. 4) of String (1 .. 1);
   type Str2 is new String (1 .. 4);
   type Flag is (Pass, Fail);
   subtype My_String is String (1 .. 4);

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("THE 'VALUE' ATTRIBUTE TAKES AN OPERAND" &
            " OF THE TYPE PREDEFINED STRING");
      end if;
      return Arg;
   end F1;

   function F is new F1 (Str2, " +1 ", Fail);
   function F is new F1 (List, " +1 ", Fail);
   function F is new F1 (Word, (' ', '+', '1', ' '), Fail);
   function F is new F1 (Str, (" ", "+", "1", " "), Fail);
   function F is new F1 (Line, (' ', '+', '1', ' '), Fail);
   function F is new F1 (My_String, " +1 ", Pass);

begin
   Test ("C87B07C", "OVERLOADED OPERANDS TO THE 'VALUE' ATTRIBUTE");

   declare
      type Int is new Integer;
      function "-" (X : Int) return Int renames "+";

   begin
      if Int'Value (F) /= -1 then
         Failed ("THE ATTRIBUTE T'VALUE MUST RETURN A VALUE" & " OF TYPE T");
      end if;
   end;

   Result;
end C87b07c;

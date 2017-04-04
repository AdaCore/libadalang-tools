-- C87B33A.ADA

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
-- THE SHORT CIRCUIT CONTROL FORMS "AND THEN" AND "OR ELSE" ARE DEFINED AS
-- BINARY BOOLEAN OPERATORS WHICH RETURN A BOOLEAN VALUE OF THE SAME TYPE
-- AS THE OPERANDS.

-- TRH  13 SEPT 82

with Report; use Report;

procedure C87b33a is

   type On is new Boolean range True .. True;
   type Off is new Boolean range False .. False;
   type Yes is new On;
   type No is new Off;
   type Bit is new Boolean;
   type Flag is (Pass, Fail);

   type Boolean is (False, True);  -- STANDARD BOOLEAN HIDDEN.

   generic
      type T is private;
      Arg : in T;
      Stat : Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("RESOLUTION INCORRECT FOR SHORT CIRCUIT " &
            "CONTROL FORMS 'AND THEN' AND 'OR ELSE' ");
      end if;
      return Arg;
   end F1;

   function A is new F1 (Boolean, True, Fail);
   function A is new F1 (No, False, Pass);
   function A is new F1 (On, True, Fail);
   function A is new F1 (Yes, True, Fail);
   function B is new F1 (Boolean, True, Fail);
   function B is new F1 (No, False, Fail);
   function B is new F1 (Off, False, Fail);
   function B is new F1 (Bit, True, Fail);
   function C is new F1 (Boolean, False, Fail);
   function C is new F1 (Yes, True, Pass);
   function C is new F1 (On, True, Fail);
   function C is new F1 (No, False, Fail);
   function D is new F1 (Boolean, False, Fail);
   function D is new F1 (Off, False, Fail);
   function D is new F1 (Yes, True, Fail);
   function D is new F1 (Bit, True, Fail);
   function E is new F1 (Boolean, False, Fail);
   function E is new F1 (Bit, True, Pass);
   function E is new F1 (Yes, True, Fail);
   function E is new F1 (No, False, Fail);
   function F is new F1 (Boolean, False, Fail);
   function F is new F1 (Bit, True, Pass);
   function F is new F1 (On, True, Fail);
   function F is new F1 (Off, False, Fail);
   function G is new F1 (Boolean, True, Fail);
   function G is new F1 (Bit, False, Pass);
   function G is new F1 (No, False, Fail);
   function G is new F1 (Yes, True, Fail);
   function H is new F1 (Boolean, True, Fail);
   function H is new F1 (Bit, False, Pass);
   function H is new F1 (Off, False, Fail);
   function H is new F1 (On, True, Fail);

begin
   Test
     ("C87B33A",
      "OVERLOADED OPERANDS FOR SHORT CIRCUIT CONTROL " &
      "FORMS 'AND THEN' AND 'OR ELSE' ");

   if (A and then B) then
      Failed ("RESOLUTION INCORRECT FOR SHORT CIRCUIT FORMS - A&B");
   end if;

   if not (C or else D) then
      Failed ("RESOLUTION INCORRECT FOR SHORT CIRCUIT FORMS - C&D");
   end if;

   if not (E and then F and then E and then F and then E and then F) then
      Failed ("RESOLUTION INCORRECT FOR SHORT CIRCUIT FORMS - E&F");
   end if;

   if (G or else H or else G or else H or else G or else H) then
      Failed ("RESOLUTION INCORRECT FOR SHORT CIRCUIT FORMS - G&H");
   end if;

   Result;
end C87b33a;

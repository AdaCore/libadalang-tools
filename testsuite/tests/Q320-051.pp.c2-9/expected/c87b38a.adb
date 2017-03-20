-- C87B38A.ADA

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

-- IN A QUALIFIED EXPRESSION, THE OPERAND MUST HAVE THE SAME TYPE
-- AS THE BASE TYPE OF THE TYPEMARK.

-- TRH  13 SEPT 82

with Report; use Report;

procedure C87b38a is

   subtype Bool is Boolean;
   type Yes is new Boolean range True .. True;
   type No is new Boolean range False .. False;
   type Bit is new Boolean;
   type Lit is (False, True);
   type Flag is (Pass, Fail);

   generic
      type T is private;
      Arg : in T;
      Stat : Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED " &
            " OPERANDS OF QUALIFIED EXPRESSIONS");
      end if;
      return Arg;
   end F1;

   function F is new F1 (Lit, False, Fail);
   function F is new F1 (Bit, True, Fail);
   function F is new F1 (Boolean, True, Pass);
   function F is new F1 (Yes, True, Fail);
   function F is new F1 (No, False, Fail);

begin
   Test ("C87B38A", "OVERLOADED OPERANDS IN QUALIFIED EXPRESSIONS ");

   declare
      B : Bool;

   begin
      B := Bool'(F);
      B := Bool'((not F) or else (F and then F));
   end;

   Result;
end C87b38a;

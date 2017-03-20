-- C87B41A.ADA

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
-- THE NAMED VARIABLE AND THE RIGHT HAND SIDE EXPRESSION
-- IN AN ASSIGNMENT STATEMENT MUST BE OF THE SAME TYPE. THIS TYPE
-- MUST NOT BE A LIMITED TYPE.

-- TRH  15 SEPT 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
-- RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURN.

with Report; use Report;

procedure C87b41a is

   type Note is (A, B, C, D, E, F, G);
   type Positive is new Integer range 1 .. Integer'Last;
   type Acc_Char is access Character;
   type Acc_Dur is access Duration;
   type Acc_Pos is access Positive;
   type Acc_Int is access Integer;
   type Acc_Bool is access Boolean;
   type Acc_Str is access String;
   type Acc_Flt is access Float;
   type Acc_Note is access Note;

   type New_Char is new Character;
   type New_Dur is new Duration;
   type New_Pos is new Positive;
   type New_Int is new Integer;
   type New_Bool is new Boolean;
   type New_Flt is new Float;
   type New_Note is new Note range A .. F;
   task type T;

   task body T is
   begin
      null;
   end T;

   function G return T is
   begin
      Failed ("LIMITED TYPES MAY NOT OCCUR IN ASSIGNMENT " & "STATEMENTS");
      return T1 : T;
   end G;

   generic
      type T is private;
      Arg : in T;
   function F1 return T;

   function F1 return T is
   begin
      Failed
        ("RESOLUTION INCORRECT - RIGHT HAND SIDE OF " &
         "ASSIGNMENT STATEMENT MUST MATCH TYPE OF VARIABLE");
      return Arg;
   end F1;

   function F is new F1 (Acc_Char, new Character);
   function F is new F1 (Acc_Dur, new Duration);
   function F is new F1 (Acc_Pos, new Positive);
   function F is new F1 (Acc_Int, new Integer);
   function F is new F1 (Acc_Bool, new Boolean);
   function F is new F1 (Acc_Str, new String (1 .. 2));
   function F is new F1 (Acc_Flt, new Float);

   function F return Acc_Note is
   begin
      return (new Note);
   end F;

   function G is new F1 (New_Char, 'G');
   function G is new F1 (New_Dur, 1.0);
   function G is new F1 (New_Pos, +10);
   function G is new F1 (New_Int, -10);
   function G is new F1 (New_Bool, True);
   function G is new F1 (New_Flt, 1.0);
   function G is new F1 (New_Note, F);

begin
   Test
     ("C87B41A",
      "OVERLOADED CONSTRUCTS ON BOTH SIDES OF THE " & "ASSIGNMENT STATEMENT");

   F.all := G;

   Result;

end C87b41a;

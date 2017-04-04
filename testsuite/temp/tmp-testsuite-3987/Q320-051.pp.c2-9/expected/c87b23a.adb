-- C87B23A.ADA

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
-- FOR AN INDEXED COMPONENT OF AN ARRAY, THE PREFIX MUST BE
-- APPROPRIATE FOR AN ARRAY TYPE. EACH EXPRESSION FOR THE INDEXED
-- COMPONENT MUST BE OF THE TYPE OF THE CORRESPONDING INDEX AND
-- THERE MUST BE ONE SUCH EXPRESSION FOR EACH INDEX POSITION OF THE
-- ARRAY TYPE.

-- TRH  15 SEPT 82
-- DSJ  07 JUNE 83

with Report; use Report;

procedure C87b23a is

   subtype Char is Character;
   type Grade is (A, B, C, D, F);
   type Note is (A, B, C, D, E, F, G);
   type Int is new Integer;
   type Pos is new Integer range 1 .. Integer'Last;
   type Nat is new Pos;
   type Bool is new Boolean;
   type Bit is new Bool;
   type Lit is (False, True);
   type Flag is (Pass, Fail);

   type Num2 is digits (2);
   type Num3 is digits (2);
   type Num4 is digits (2);

   type A1 is
     array (Pos'(1) .. 5, Note'(A) .. D, Bool'(False) .. True) of Float;
   type A2 is array (Int'(1) .. 5, Note'(A) .. D, Bit'(False) .. True) of Num2;
   type A3 is
     array (Pos'(1) .. 5, Grade'(A) .. D, Bool'(False) .. True) of Num3;
   type A4 is array (Nat'(1) .. 5, Note'(A) .. D, Lit'(False) .. True) of Num4;

   Obj1 : A1 := (others => (others => (others => 0.0)));
   Obj2 : A2 := (others => (others => (others => 0.0)));
   Obj3 : A3 := (others => (others => (others => 0.0)));
   Obj4 : A4 := (others => (others => (others => 0.0)));

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("PREFIX OR INDEX IS NOT APPROPRIATE FOR" & " INDEXED COMPONENT");
      end if;
      return Arg;
   end F1;

   function A is new F1 (A1, Obj1, Pass);
   function A is new F1 (A2, Obj2, Fail);
   function A is new F1 (A3, Obj3, Fail);
   function A is new F1 (A4, Obj4, Fail);

begin
   Test ("C87B23A", "OVERLOADED ARRAY INDEXES");

   declare
      F1 : Float := A (3, C, True);

   begin
      null;
   end;

   Result;
end C87b23a;

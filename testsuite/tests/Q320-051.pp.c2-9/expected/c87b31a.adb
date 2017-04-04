-- C87B31A.ADA

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
-- IF THE TYPE OF AN AGGREGATE IS A ONE-DIMENSIONAL ARRAY TYPE THEN EACH CHOICE
-- MUST SPECIFY VALUES OF THE INDEX TYPE, AND THE EXPRESSION OF EACH COMPONENT
-- ASSOCIATION MUST BE OF THE COMPONENT TYPE.

-- TRH  8 AUG 82
-- DSJ 15 JUN 83
-- JRK  2 FEB 84
-- JBG 4/23/84

with Report; use Report;

procedure C87b31a is

   type Letter is new Character range 'A' .. 'Z';
   type Note is (A, B, C, D, E, F, G, H);
   type Str is new String (1 .. 1);
   type Bit is new Boolean;
   type Yes is new Boolean range True .. True;
   type No is new Boolean range False .. False;
   type Boolean is (False, True);
   type List is array (Character range <>) of Bit;
   type Flag is (Pass, Fail);

   subtype List_A is List ('A' .. 'A');
   subtype List_E is List ('E' .. 'E');
   subtype List_Ae is List ('A' .. 'E');

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("RESOLUTION INCORRECT FOR EXPRESSIONS " & "IN ARRAY AGGREGATES");
      end if;
      return Arg;
   end F1;

   function F is new F1 (Boolean, False, Fail);
   function F is new F1 (Yes, True, Fail);
   function F is new F1 (No, False, Fail);
   function F is new F1 (Bit, True, Pass);

   function G is new F1 (Character, 'A', Pass);
   function G is new F1 (Letter, 'A', Fail);
   function G is new F1 (Str, "A", Fail);

   function H is new F1 (Character, 'E', Pass);
   function H is new F1 (Letter, 'E', Fail);
   function H is new F1 (Str, "E", Fail);

begin
   Test ("C87B31A", "OVERLOADED EXPRESSIONS IN ARRAY AGGREGATES");

   declare
      L1, L2 : List_A  := (others => False);
      L3, L4 : List_E  := (others => False);
      L5, L6 : List_Ae := (others => False);
      L7, L8 : List_Ae := (others => False);

   begin
      L1 := ('A' => F);
      L2 := (G => F);
      L3 := ('E' => F);
      L4 := (H => F);
      L5 := ('A' .. 'E' => F);
      L6 := (F, F, F, F, F);
      L7 := (F, F, F, others => F);
      L8 := List_Ae'('E' => F, 'B' => F, others => F);

      if L1 /= List_A'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L1");
      end if;
      if L2 /= List_A'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L2");
      end if;
      if L3 /= List_E'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L3");
      end if;
      if L4 /= List_E'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L4");
      end if;
      if L5 /= List_Ae'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L5");
      end if;
      if L6 /= List_Ae'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L6");
      end if;
      if L7 /= List_Ae'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L7");
      end if;
      if L8 /= List_Ae'(others => True) then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED" &
            " EXPRESSIONS IN ARRAY AGGREGATES - L8");
      end if;
   end;

   Result;
end C87b31a;

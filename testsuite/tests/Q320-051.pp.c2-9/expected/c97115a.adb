-- C97115A.ADA

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
-- CHECK WHETHER AN ENTRY FAMILY INDEX EXPRESSION FOLLOWING AN OPEN GUARD
-- IS EVALUATED DIRECTLY AFTER THE GUARD, OR ONLY AFTER ALL GUARDS HAVE BEEN
-- EVALUATED, OR IN SOME MIXED ORDER SUCH THAT INDEX EXPRESSIONS ARE EVALUATED
-- AFTER THEIR GUARDS ARE DETERMINED TO BE OPEN.

-- RM 5/11/82
-- SPS 11/21/82
-- JBG 10/24/83
-- PWN 09/11/94 REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C97115a is

   -- THE TASK WILL HAVE LAST PRIORITY ( PRIORITY'FIRST )

   Eval_Order : String (1 .. 6) := (1 .. 6 => '*');
   Eval_Ord   : String (1 .. 6) := (1 .. 6 => '*');
   Index      : Integer         := 0;

   function F1 (X : Integer) return Integer is
   begin
      Index              := Index + 1;
      Eval_Order (Index) := 'F';    -- 123: FGH
      Eval_Ord (Index)   := 'G';    -- 123: GGG ( 'G' FOR 'GUARD' )
      return (Ident_Int (7));
   end F1;

   function F2 (X : Integer) return Integer is
   begin
      Index              := Index + 1;
      Eval_Order (Index) := 'G';
      Eval_Ord (Index)   := 'G';
      return (Ident_Int (7));
   end F2;

   function F3 (X : Integer) return Integer is
   begin
      Index              := Index + 1;
      Eval_Order (Index) := 'H';
      Eval_Ord (Index)   := 'G';
      return (Ident_Int (7));
   end F3;

   function I1 (X : Integer) return Boolean is
   begin
      Index              := Index + 1;
      Eval_Order (Index) := 'A';  -- 123: ABC
      Eval_Ord (Index)   := 'I';  -- 123: III ( 'I' FOR 'INDEX' )
      return (Ident_Bool (True));    -- (THAT'S ENTRY-FAMILY INDEX)
   end I1;

   function I2 (X : Integer) return Boolean is
   begin
      Index              := Index + 1;
      Eval_Order (Index) := 'B';
      Eval_Ord (Index)   := 'I';
      return (Ident_Bool (True));
   end I2;

   function I3 (X : Integer) return Boolean is
   begin
      Index              := Index + 1;
      Eval_Order (Index) := 'C';
      Eval_Ord (Index)   := 'I';
      return (Ident_Bool (True));
   end I3;

   function Pos_Of (Func : Character) return Integer is
   begin
      for I in Eval_Order'Range loop
         if Eval_Order (I) = Func then
            return I;
         end if;
      end loop;
      Failed ("DID NOT FIND LETTER " & Func);
      return 0;
   end Pos_Of;

begin

   Test
     ("C97115A",
      "CHECK THAT THE INDEX EXPRESSIONS ARE" &
      " EVALUATED AFTER THE GUARDS BUT" &
      " BEFORE THE RENDEZVOUS IS ATTEMPTED");

   declare

      task T is

         entry E (Boolean);
         entry E1;

      end T;

      task body T is
      begin

         while E1'Count = 0  -- IF  E1  NOT YET CALLED, THEN GIVE
         loop                 --     THE MAIN TASK AN OPPORTUNITY
            delay 10.01;   --     TO ISSUE THE CALL.
         end loop;

         select

            accept E1;

         or
 when 6 + F1 (7) = 13 =>
            accept E (I1 (17));

         or
 when 6 + F2 (7) = 13 =>
            accept E (I2 (17));

         or
 when 6 + F3 (7) = 13 =>
            accept E (I3 (17));

         end select;

      end T;

   begin

      T.E1;

   end; -- END OF BLOCK CONTAINING THE ENTRY CALLS

   Comment ("GUARD AND INDEX FUNCTIONS WERE CALLED IN ORDER " & Eval_Order);
   Comment
     ("GUARD AND INDEX EXPRESSIONS WERE EVALUATED IN THE " & "ORDER " &
      Eval_Ord);

   if Pos_Of ('F') > Pos_Of ('A') or Pos_Of ('G') > Pos_Of ('B') or
     Pos_Of ('H') > Pos_Of ('C') then
      Failed ("AN INDEX EXPRESSION WAS EVALUATED TOO EARLY");
   end if;

   Result;

end C97115a;

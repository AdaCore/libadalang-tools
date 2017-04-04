-- C43209A.ADA

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
--     CHECK THAT A STRING LITERAL IS ALLOWED IN A MULTIDIMENSIONAL
--     ARRAY AGGREGATE AT THE PLACE OF A ONE DIMENSIONAL ARRAY OF
--     CHARACTER TYPE.

-- HISTORY:
--     DHH 08/12/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43209a is

   type Multi_Array is array (1 .. 2, 1 .. 3, 1 .. 6) of Character;

begin
   Test
     ("C43209A",
      "CHECK THAT A STRING LITERAL IS ALLOWED IN A " &
      "MULTIDIMENSIONAL ARRAY AGGREGATE AT THE PLACE " &
      "OF A ONE DIMENSIONAL ARRAY OF CHARACTER TYPE");

   declare
      X : Multi_Array :=
        ((('A', 'B', 'C', 'D', 'E', 'F'),
          ('G', 'H', 'I', 'J', 'K', 'L'),
          ('M', 'N', 'O', 'P', 'Q', 'R')),
         (('S', 'T', 'U', 'V', 'W', 'X'),
          ('W', 'Z', 'A', 'B', 'C', 'D'),
          "WHOZAT"));

      Y : Multi_Array :=
        (("WHOZAT",
          ('A', 'B', 'C', 'D', 'E', 'F'),
          ('G', 'H', 'I', 'J', 'K', 'L')),
         (('M', 'N', 'O', 'P', 'Q', 'R'),
          ('S', 'T', 'U', 'V', 'W', 'X'),
          ('W', 'Z', 'A', 'B', 'C', 'D')));

   begin
      if X (Ident_Int (2), Ident_Int (3), Ident_Int (6)) /=
        Y (Ident_Int (1), Ident_Int (1), Ident_Int (6))
      then
         Failed ("INITIALIZATION FAILURE");
      end if;
   end;

   declare
      procedure Fix_Agg (T : Multi_Array) is
      begin
         if T (Ident_Int (2), Ident_Int (2), Ident_Int (5)) /=
           T (Ident_Int (1), Ident_Int (1), Ident_Int (1))
         then
            Failed ("SUBPROGRAM FAILURE");
         end if;
      end Fix_Agg;
   begin
      Fix_Agg
        ((
          ("WHOZAT",
           ('A', 'B', 'C', 'D', 'E', 'F'),
           ('G', 'H', 'I', 'J', 'K', 'L')),
          (('M', 'N', 'O', 'P', 'Q', 'R'),
           ('S', 'T', 'U', 'V', 'W', 'X'),
           ('W', 'Z', 'A', 'B', 'C', 'D'))));

   end;

   declare

      Y : constant Multi_Array :=
        (("WHOZAT",
          ('A', 'B', 'C', 'D', 'E', 'F'),
          ('G', 'H', 'I', 'J', 'K', 'L')),
         (('M', 'N', 'O', 'P', 'Q', 'R'),
          ('S', 'T', 'U', 'V', 'W', 'X'),
          ('W', 'Z', 'A', 'B', 'C', 'D')));

   begin
      if Y (Ident_Int (2), Ident_Int (2), Ident_Int (5)) /=
        Y (Ident_Int (1), Ident_Int (1), Ident_Int (1))
      then
         Failed ("CONSTANT FAILURE");
      end if;
   end;

   declare
   begin
      if Multi_Array'
          ((1 =>
              (('A', 'B', 'C', 'D', 'E', 'F'),
               ('G', 'H', 'I', 'J', 'K', 'L'),
               ('M', 'N', 'O', 'P', 'Q', 'R')),
            2 =>
              (('S', 'T', 'U', 'V', 'W', 'X'),
               ('W', 'Z', 'A', 'B', 'C', 'D'),
               "WHOZAT"))) =
        Multi_Array'
          ((1 =>
              (1 => "WHOZAT",
               2 => ('A', 'B', 'C', 'D', 'E', 'F'),
               3 => ('G', 'H', 'I', 'J', 'K', 'L')),
            2 =>
              (1 => ('M', 'N', 'O', 'P', 'Q', 'R'),
               2 => ('S', 'T', 'U', 'V', 'W', 'X'),
               3 => ('W', 'Z', 'A', 'B', 'C', 'D'))))
      then
         Failed ("EQUALITY OPERATOR FAILURE");
      end if;
   end;

   declare
      subtype Sm is Integer range 1 .. 10;
      type Unconstr is array (Sm range <>, Sm range <>) of Character;

      function Func (X : Sm) return Unconstr is
      begin
         if Equal (X, X) then
            return (1 => "WHEN", 2 => "WHAT");
         else
            return ("    ", "    ");
         end if;
      end Func;

   begin
      if Func (1) /= Func (2) then
         Failed ("UNCONSTRAINED FUNCTION RETURN FAILURE");
      end if;
   end;

   Result;
end C43209a;

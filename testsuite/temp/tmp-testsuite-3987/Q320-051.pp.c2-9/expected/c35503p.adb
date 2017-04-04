-- C35503P.ADA

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
--     CHECK THAT 'FIRST' AND 'LAST' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ARGUMENT IS AN
--     INTEGER TYPE.

-- HISTORY:
--     RJW 03/24/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35503p is

begin
   Test
     ("C35503P",
      "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "GENERIC FORMAL DISCRETE TYPE WHOSE ARGUMENT " &
      "IS AN INTEGER TYPE");

   declare

      type Int is range -6 .. 6;
      subtype Sint is Int range Int (Ident_Int (-4)) .. Int (Ident_Int (4));
      subtype Noint is Int range Int (Ident_Int (1)) .. Int (Ident_Int (-1));

      generic
         type I is (<>);
         F, L : I;
      procedure P (Str : String);

      procedure P (Str : String) is
      begin
         if I'First /= F then
            Failed ("INCORRECT 'FIRST' FOR " & Str);
         end if;
         if I'Last /= L then
            Failed ("INCORRECT 'LAST' FOR " & Str);
         end if;
      end P;

      generic
         type I is (<>);
         F, L : I;
      procedure Q;

      procedure Q is
         subtype Si is I;
      begin
         if Si'First /= F then
            Failed ("INCORRECT VALUE FOR INTEGER'FIRST");
         end if;
         if Si'Last /= L then
            Failed ("INCORRECT VALUE FOR INTEGER'LAST");
         end if;
      end Q;

      generic
         type I is (<>);
      procedure R;

      procedure R is
         subtype Si is I;
      begin
         if Si'First /= Si'Val (Ident_Int (1)) then
            Failed ("INCORRECT VALUE FOR NOINT'FIRST");
         end if;
         if Si'Last /= Si'Val (Ident_Int (-1)) then
            Failed ("INCORRECT VALUE FOR NOINT'LAST");
         end if;
      end R;

      procedure P1 is new P (I => Int, F => -6, L => 6);
      procedure P2 is new P (I => Sint, F => -4, L => 4);
      procedure Q1 is new Q
        (I => Integer,
         F => Integer'First,
         L => Integer'Last);
      procedure R1 is new R (I => Noint);

   begin
      P1 ("INT");
      P2 ("SINT");
      Q1;
      R1;
   end;

   Result;
end C35503p;

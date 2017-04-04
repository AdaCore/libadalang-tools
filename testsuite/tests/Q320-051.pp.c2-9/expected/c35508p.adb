-- C35508P.ADA

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
--     PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER
--     IS A BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35508p is

begin
   Test
     ("C35508P",
      "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL " &
      "PARAMETER IS A BOOLEAN TYPE");
   declare
      subtype Tbool is Boolean range Ident_Bool (True) .. Ident_Bool (True);
      subtype Fbool is Boolean range Ident_Bool (False) .. Ident_Bool (False);
      subtype Nobool is Boolean range Ident_Bool (True) .. Ident_Bool (False);
      type Newbool is new Boolean;

      generic
         type Bool is (<>);
         F, L : Bool;
      procedure P (Str : String);

      procedure P (Str : String) is
      begin
         if Bool'First /= F then
            Failed ("WRONG VALUE FOR " & Str & "'FIRST");
         end if;
         if Bool'Last /= L then
            Failed ("WRONG VALUE FOR " & Str & "'LAST");
         end if;
      end P;

      generic
         type Bool is (<>);
      procedure Q;

      procedure Q is
      begin
         if Bool'First /= Bool'Val (Ident_Int (1)) then
            Failed ("WRONG 'FIRST FOR NOBOOL");
         end if;
         if Bool'Last /= Bool'Val (Ident_Int (0)) then
            Failed ("WRONG 'LAST FOR NOBOOL");
         end if;
      end Q;

      generic
         type Bool is (<>);
         F, L : Bool;
      procedure R;

      procedure R is
         subtype Sbool is Bool range Bool'Val (0) .. Bool'Val (1);
      begin
         if Sbool'First /= F then
            Failed ("WRONG VALUE FOR BOOLEAN'FIRST AS " & "SUBTYPE ");
         end if;
         if Sbool'Last /= L then
            Failed ("WRONG VALUE FOR BOOLEAN'LAST AS " & "SUBTYPE");
         end if;
      end R;

      procedure P1 is new P
        (Bool => Boolean,
         F    => Ident_Bool (False),
         L    => Ident_Bool (True));

      procedure P2 is new P
        (Bool => Tbool,
         F    => Ident_Bool (True),
         L    => Ident_Bool (True));

      procedure P3 is new P
        (Bool => Fbool,
         F    => Ident_Bool (False),
         L    => Ident_Bool (False));

      procedure P4 is new P (Bool => Newbool, F => False, L => True);

      procedure Q1 is new Q (Bool => Nobool);

      procedure R1 is new R (Bool => Boolean, F => False, L => True);

   begin
      P1 ("BOOLEAN");
      P2 ("TBOOL");
      P3 ("FBOOL");
      P4 ("NEWBOOL");
      Q1;
      R1;
   end;

   Result;
end C35508p;

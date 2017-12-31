-- C35503O.ADA

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
--     PREFIX IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35503o is

begin
   Test
     ("C35503O",
      "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS AN " & "INTEGER TYPE");

   declare
      subtype Sinteger is Integer;
      subtype Small is Integer range Ident_Int (-10) .. Ident_Int (10);
      subtype Nointeger is Integer range Ident_Int (5) .. Ident_Int (-7);

      type Int is range -6 .. 6;
      subtype Sint is Int range Int (Ident_Int (-4)) .. Int (Ident_Int (4));
      subtype Noint is Int range Int (Ident_Int (1)) .. Int (Ident_Int (-1));
      type Newint is new Integer range Ident_Int (-9) .. Ident_Int (-2);
      subtype Snewint is Newint range -7 .. -5;
      subtype Nonewint is Newint range 3 .. -15;

   begin
      if Sinteger'First /= Integer'First then
         Failed ("WRONG VALUE FOR SINTEGER'FIRST");
      end if;
      if Sinteger'Last /= Integer'Last then
         Failed ("WRONG VALUE FOR SINTEGER'LAST");
      end if;

      if Small'First /= -10 then
         Failed ("WRONG VALUE FOR SMALL'FIRST");
      end if;
      if Small'Last /= 10 then
         Failed ("WRONG VALUE FOR SMALL'LAST");
      end if;

      if Nointeger'First /= 5 then
         Failed ("WRONG VALUE FOR NOINTEGER'FIRST");
      end if;
      if Nointeger'Last /= -7 then
         Failed ("WRONG VALUE FOR NOINTEGER'LAST");
      end if;

      if Int'First /= -6 then
         Failed ("WRONG VALUE FOR INT'FIRST");
      end if;
      if Int'Last /= 6 then
         Failed ("WRONG VALUE FOR INT'LAST");
      end if;

      if Sint'First /= -4 then
         Failed ("WRONG VALUE FOR SINT'FIRST");
      end if;
      if Sint'Last /= 4 then
         Failed ("WRONG VALUE FOR SINT'LAST");
      end if;

      if Noint'First /= 1 then
         Failed ("WRONG VALUE FOR NOINT'FIRST");
      end if;
      if Noint'Last /= -1 then
         Failed ("WRONG VALUE FOR NOINT'LAST");
      end if;

      if Newint'First /= -9 then
         Failed ("WRONG VALUE FOR NEWINT'FIRST");
      end if;
      if Newint'Last /= -2 then
         Failed ("WRONG VALUE FOR NEWINT'LAST");
      end if;

      if Snewint'First /= -7 then
         Failed ("WRONG VALUE FOR SNEWINT'FIRST");
      end if;
      if Snewint'Last /= -5 then
         Failed ("WRONG VALUE FOR SNEWINT'LAST");
      end if;

      if Nonewint'First /= 3 then
         Failed ("WRONG VALUE FOR NONEWINT'FIRST");
      end if;
      if Nonewint'Last /= -15 then
         Failed ("WRONG VALUE FOR NONEWINT'LAST");
      end if;
   end;

   Result;
end C35503o;

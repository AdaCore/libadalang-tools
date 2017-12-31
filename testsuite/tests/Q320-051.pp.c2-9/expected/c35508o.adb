-- C35508O.ADA

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
--     PREFIX IS A BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35508o is

begin
   Test
     ("C35508O",
      "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " & "BOOLEAN TYPE");

   declare
      subtype Tbool is Boolean range Ident_Bool (True) .. Ident_Bool (True);
      subtype Fbool is Boolean range Ident_Bool (False) .. Ident_Bool (False);
      subtype Nobool is Boolean range Ident_Bool (True) .. Ident_Bool (False);
      type Newbool is new Boolean;
      type Nil is new Boolean range Ident_Bool (True) .. Ident_Bool (False);

   begin
      if Ident_Bool (Boolean'First) /= False then
         Failed ("WRONG VALUE FOR BOOLEAN'FIRST");
      end if;
      if Ident_Bool (Boolean'Last) /= True then
         Failed ("WRONG VALUE FOR BOOLEAN'LAST");
      end if;

      if Tbool'First /= True then
         Failed ("WRONG VALUE FOR TBOOL'FIRST");
      end if;
      if Tbool'Last /= True then
         Failed ("WRONG VALUE FOR TBOOL'LAST");
      end if;

      if Fbool'First /= False then
         Failed ("WRONG VALUE FOR FBOOL'FIRST");
      end if;
      if Fbool'Last /= False then
         Failed ("WRONG VALUE FOR FBOOL'LAST");
      end if;

      if Nobool'First /= True then
         Failed ("WRONG VALUE FOR NOBOOL'FIRST");
      end if;
      if Nobool'Last /= False then
         Failed ("WRONG VALUE FOR NOBOOL'LAST");
      end if;

      if Newbool'First /= False then
         Failed ("WRONG VALUE FOR NEWBOOL'FIRST");
      end if;
      if Newbool'Last /= True then
         Failed ("WRONG VALUE FOR NEWBOOL'LAST");
      end if;
      if Nil'First /= True then
         Failed ("WRONG VALUE FOR NIL'FIRST");
      end if;
      if Nil'Last /= False then
         Failed ("WRONG VALUE FOR NIL'LAST");
      end if;

   end;

   Result;
end C35508o;

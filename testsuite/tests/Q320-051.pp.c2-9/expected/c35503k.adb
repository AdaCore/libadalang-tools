-- C35503K.ADA

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
--     CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     PWN 11/30/94 REMOVED ATTRIBUTE TESTS ILLEGAL IN ADA 9X.

with System; use System;
with Report; use Report;

procedure C35503k is

begin
   Test
     ("C35503K",
      "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
      "INTEGER TYPE");

   declare
      type Int is range -6 .. 6;
      subtype Sint is Int range -4 .. 4;

      procedure P (I : Integer; Str : String) is
      begin
         begin
            if Integer'Pos (I) /= I then
               Failed ("WRONG POS FOR " & Str);
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED FOR POS OF " & Str);
         end;
         begin
            if Integer'Val (I) /= I then
               Failed ("WRONG VAL FOR " & Str);
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED FOR VAL OF " & Str);
         end;
      end P;

   begin
      P (Integer'First, "INTEGER'FIRST");
      P (Integer'Last, "INTEGER'LAST");
      P (0, "'0'");

      for I in Int'First .. Int'Last loop
         begin
            if Sint'Pos (I) /= I then
               Failed ("WRONG POS FOR " & Int'Image (I));
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED FOR POS OF " & Int'Image (I));
         end;
         begin
            if Sint'Val (I) /= I then
               Failed ("WRONG VAL FOR " & Int'Image (I));
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED FOR VAL OF " & Int'Image (I));
         end;
      end loop;

      begin
         if Int'Val (Integer'(0)) /= 0 then
            Failed ("WRONG VAL FOR INT WITH INTEGER");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED FOR VAL OF " & "INT WITH INTEGER");
      end;

      begin
         if Integer'Val (Int'(0)) /= 0 then
            Failed ("WRONG VAL FOR INTEGER WITH INT");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED FOR VAL OF " & "INTEGER WITH INT");
      end;
   end;

   Result;
end C35503k;

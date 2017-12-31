-- C35503G.ADA

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
--     CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULT WHEN THE
--     PREFIX IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35503g is

begin
   Test
     ("C35503G",
      "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
      "CORRECT RESULT WHEN THE PREFIX IS AN " & "INTEGER TYPE");

   declare
      type Int is range -6 .. 6;
      subtype Sint is Int range -4 .. 4;

   begin

      for I in Int'First + 1 .. Int'Last loop
         begin
            if Sint'Pred (I) /= I - 1 then
               Failed ("WRONG SINT'PRED FOR " & Int'Image (I));
            end if;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED FOR " & "SINT'PRED OF " & Int'Image (I));
         end;
      end loop;

      for I in Int'First .. Int'Last - 1 loop
         begin
            if Sint'Succ (I) /= I + 1 then
               Failed ("WRONG SINT'SUCC FOR " & Int'Image (I));
            end if;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED FOR " & "SINT'SUCC OF " & Int'Image (I));
         end;
      end loop;

   end;

   declare
      subtype Intrange is Integer range Ident_Int (-6) .. Ident_Int (6);
      subtype Sinteger is Integer range Ident_Int (-4) .. Ident_Int (4);

   begin
      for I in Intrange loop
         begin
            if Sinteger'Pred (I) /= I - Ident_Int (1) then
               Failed ("WRONG SINTEGER'PRED FOR " & Integer'Image (I));
            end if;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED FOR " & "SINTEGER'PRED OF " &
                  Integer'Image (I));
         end;
         begin
            if Sinteger'Succ (I) /= I + Ident_Int (1) then
               Failed ("WRONG SINTEGER'SUCC FOR " & Integer'Image (I));
            end if;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED FOR " & "SINTEGER'SUCC OF " &
                  Integer'Image (I));
         end;
      end loop;

   end;

   Result;
end C35503g;

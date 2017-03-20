-- C35507G.ADA

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
--     CHECK THAT THE ATTRIBUTES 'PRED' AND 'SUCC' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.

-- HISTORY:
--     RJW 06/03/86  CREATED ORIGINAL TEST.
--     JET 08/13/87  REMOVED TESTS INTENDED FOR C35505F.

with Report; use Report;

procedure C35507g is

   type Char is ('A', B);

   type Newchar is new Char;

   function Ident (Ch : Char) return Char is
   begin
      return Char'Val (Ident_Int (Char'Pos (Ch)));
   end Ident;

   function Ident (Ch : Newchar) return Newchar is
   begin
      return Newchar'Val (Ident_Int (Newchar'Pos (Ch)));
   end Ident;

begin

   Test
     ("C35507G",
      "CHECK THAT THE ATTRIBUTES 'PRED' AND " &
      "'SUCC' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A CHARACTER TYPE");

   begin
      if Char'Succ ('A') /= B then
         Failed ("INCORRECT VALUE FOR CHAR'SUCC('A')");
      end if;

      if Char'Pred (Ident (B)) /= 'A' then
         Failed ("INCORRECT VALUE FOR CHAR'PRED (IDENT (B))");
      end if;
   end;

   begin
      if Newchar'Succ (Ident ('A')) /= B then
         Failed ("INCORRECT VALUE FOR " & "IDENT (NEWCHAR'SUCC('A'))");
      end if;

      if Newchar'Pred (B) /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'PRED(B)");
      end if;
   end;

   for Ch in Character'Val (1) .. Character'Val (127) loop
      if Character'Pred (Ch) /= Character'Val (Character'Pos (Ch) - 1) then
         Failed
           ("INCORRECT VALUE FOR CHARACTER'PRED OF " & Character'Image (Ch));
      end if;
   end loop;

   for Ch in Character'Val (0) .. Character'Val (126) loop
      if Character'Succ (Ch) /= Character'Val (Character'Pos (Ch) + 1) then
         Failed
           ("INCORRECT VALUE FOR CHARACTER'SUCC OF " & Character'Image (Ch));
      end if;
   end loop;

   Result;

end C35507g;

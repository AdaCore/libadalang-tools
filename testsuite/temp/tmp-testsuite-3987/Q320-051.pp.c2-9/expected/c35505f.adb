-- C35505F.ADA

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
--     CHECK THAT CONSTRAINT ERROR IS RAISED BY THE ATTRIBUTES
--     'PRED' AND 'SUCC' WHEN THE PREFIX IS A CHARACTER TYPE
--     AND THE RESULT IS OUTSIDE OF THE BASE TYPE.

-- HISTORY:
--     JET 08/18/87  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C35505f is

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
     ("C35505F",
      "CHECK THAT CONSTRAINT ERROR IS RAISED BY " &
      "THE ATTRIBUTES 'PRED' AND 'SUCC' WHEN THE " &
      "PREFIX IS A CHARACTER TYPE AND THE RESULT " &
      "IS OUTSIDE OF THE BASE TYPE");

   begin
      if Char'Pred (Ident ('A')) = 'A' then
         Failed ("NO EXCEPTION RAISED " & "FOR CHAR'PRED (IDENT ('A')) - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "FOR CHAR'PRED (IDENT ('A')) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR CHAR'PRED (IDENT ('A'))");
   end;

   begin
      if Char'Succ (Ident (B)) = B then
         Failed ("NO EXCEPTION RAISED " & "FOR CHAR'SUCC (IDENT (B)) - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "FOR CHAR'SUCC (IDENT (B)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR CHAR'SUCC (IDENT (B))");
   end;

   begin
      if Newchar'Pred (Ident ('A')) = 'A' then
         Failed
           ("NO EXCEPTION RAISED " & "FOR NEWCHAR'PRED (IDENT ('A')) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED " & "FOR NEWCHAR'PRED (IDENT ('A')) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR NEWCHAR'PRED (IDENT ('A'))");
   end;

   begin
      if Newchar'Succ (Ident (B)) = 'A' then
         Failed ("NO EXCEPTION RAISED " & "FOR NEWCHAR'SUCC (IDENT (B)) - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "FOR NEWCHAR'SUCC (IDENT (B)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR NEWCHAR'SUCC (IDENT (B))");
   end;

   begin
      if Character'Pred (Ident_Char (Character'Base'First)) = 'A' then
         Failed
           ("NO EXCEPTION RAISED " &
            "FOR CHARACTER'PRED " &
            "(IDENT_CHAR (CHARACTER'BASE'FIRST)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED " &
            "FOR CHARACTER'PRED " &
            "(IDENT_CHAR (CHARACTER'BASE'FIRST)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'PRED " &
            "(IDENT_CHAR (CHARACTER'BASE'FIRST))");
   end;

   begin
      if Character'Succ (Ident_Char (Character'Base'Last)) = 'Z' then
         Failed
           ("NO EXCEPTION RAISED " &
            "FOR CHARACTER'SUCC " &
            "(IDENT_CHAR (CHARACTER'BASE'LAST)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED " &
            "FOR CHARACTER'SUCC " &
            "(IDENT_CHAR (CHARACTER'BASE'LAST)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'SUCC " &
            "(IDENT_CHAR (CHARACTER'BASE'LAST))");
   end;

   Result;

end C35505f;

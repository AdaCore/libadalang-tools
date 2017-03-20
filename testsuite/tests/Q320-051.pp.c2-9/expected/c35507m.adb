-- C35507M.ADA

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
--     CHECK THAT THE ATTRIBUTES 'POS' AND 'VAL' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE WITH AN ENUMERATION
--     REPRESENTATION CLAUSE.

-- HISTORY:
--     RJW 06/03/86  CREATED ORIGINAL TEST
--     JLH 07/28/87  MODIFIED FUNCTION IDENT.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure C35507m is

   type Char is ('A', B);
   for Char use ('A' => 4, B => 5);

   type Newchar is new Char;

   function Ident (Ch : Char) return Char is
   begin
      if Equal (3, 3) then
         return Ch;
      else
         return 'A';
      end if;
   end Ident;

   function Ident (Ch : Newchar) return Newchar is
   begin
      if Equal (3, 3) then
         return Ch;
      else
         return 'A';
      end if;
   end Ident;

begin

   Test
     ("C35507M",
      "CHECK THAT THE ATTRIBUTES 'POS' AND " &
      "'VAL' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A CHARACTER TYPE WITH AN " &
      "ENUMERATION REPESENTATION CLAUSE");

   begin
      if Char'Pos ('A') /= 0 then
         Failed ("INCORRECT VALUE FOR CHAR'POS('A')");
      end if;

      if Char'Pos (B) /= 1 then
         Failed ("INCORRECT VALUE FOR CHAR'POS(B)");
      end if;

      if Char'Val (0) /= 'A' then
         Failed ("INCORRECT VALUE FOR CHAR'VAL(0)");
      end if;

      if Char'Val (1) /= B then
         Failed ("INCORRECT VALUE FOR CHAR'VAL(1)");
      end if;
   end;

   begin
      if Newchar'Pos ('A') /= 0 then
         Failed ("INCORRECT VALUE FOR NEWCHAR'POS('A')");
      end if;

      if Newchar'Pos (B) /= 1 then
         Failed ("INCORRECT VALUE FOR NEWCHAR'POS(B)");
      end if;

      if Newchar'Val (0) /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'VAL(0)");
      end if;

      if Newchar'Val (1) /= B then
         Failed ("INCORRECT VALUE FOR NEWCHAR'VAL(1)");
      end if;
   end;

   begin
      if Char'Pos (Ident ('A')) /= 0 then
         Failed ("INCORRECT VALUE FOR CHAR'POS('A') WITH " & "IDENT");
      end if;

      if Newchar'Pos (Ident (B)) /= 1 then
         Failed ("INCORRECT VALUE FOR NEWCHAR'POS(B) WITH " & "IDENT");
      end if;

      if Ident (Newchar'Val (Ident_Int (0))) /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'VAL(0) WITH " & "IDENT");
      end if;

      if Ident (Char'Val (Ident_Int (1))) /= B then
         Failed ("INCORRECT VALUE FOR CHAR'VAL(1) WITH IDENT");
      end if;
   end;

   begin
      if Char'Val (Ident_Int (2)) = B then
         Failed ("NO EXCEPTION RAISED FOR " & "CHAR'VAL (IDENT_INT(2)) - 1");
      else
         Failed ("NO EXCEPTION RAISED FOR " & "CHAR'VAL (IDENT_INT(2)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "CHAR'VAL (IDENT_INT(2))");
   end;

   begin
      if Newchar'Val (Ident_Int (-1)) = 'A' then
         Failed
           ("NO EXCEPTION RAISED " & "FOR NEWCHAR'VAL (IDENT_INT (-1)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED " & "FOR NEWCHAR'VAL (IDENT_INT (-1)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " & "FOR NEWCHAR'VAL (IDENT_INT (-1))");
   end;

   Result;
end C35507m;

-- C41307D.ADA

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
-- CHECK THAT L.R IS ALLOWED INSIDE A PACKAGE, GENERIC PACKAGE, SUBPROGRAM,
-- GENERIC SUBPROGRAM, TASK, BLOCK, LOOP, OR AN ACCEPT STATEMENT NAMED L, IF
-- R IS DECLARED INSIDE THE UNIT.

-- TBN 12/15/86

with Report; use Report;
procedure C41307d is

begin
   Test
     ("C41307D",
      "CHECK THAT L.R IS ALLOWED INSIDE A PACKAGE, " &
      "GENERIC PACKAGE, SUBPROGRAM, GENERIC " &
      "SUBPROGRAM, TASK, BLOCK, LOOP, OR AN ACCEPT " &
      "STATEMENT NAMED L, IF R IS DECLARED INSIDE " &
      "THE UNIT");
   declare
      package L is
         R : Integer := 5;
         A : Integer := L.R;
      end L;

      package body L is
         B : Integer := L.R + 1;
      begin
         if Ident_Int (A) /= 5 or Ident_Int (B) /= 6 then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
         end if;
      end L;

      generic
         S : Integer;
      package M is
         X : Integer := M.S;
      end M;

      package body M is
         Y : Integer := M.S + 1;
      begin
         if Ident_Int (X) /= 2 or
           Ident_Int (Y) /= 3 or
           Ident_Int (M.X) /= 2
         then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
         end if;
      end M;

      package Q is new M (2);
   begin
      if Ident_Int (Q.X) /= 2 then
         Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
      end if;
   end;
   -------------------------------------------------------------------

   declare
      Ch : Character := '6';

      procedure L (R : in out Character) is
         A : Character := L.R;
      begin
         if Ident_Char (L.A) /= '6' then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
         end if;
         L.R := Ident_Char ('7');
      end L;

      generic
         S : Character;
      procedure M;

      procedure M is
         T : Character := M.S;
      begin
         if Ident_Char (T) /= '3' or Ident_Char (M.S) /= '3' then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
         end if;
      end M;

      procedure P1 is new M ('3');

   begin
      L (Ch);
      if Ch /= Ident_Char ('7') then
         Failed ("INCORRECT RESULTS RETURNED FROM PROCEDURE - 6");
      end if;
      P1;
   end;
   -------------------------------------------------------------------

   declare
      Int : Integer := 3;

      function L (R : Integer) return Integer is
         A : Integer := L.R;
      begin
         if Ident_Int (L.A) /= Ident_Int (3) then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
         end if;
         return Ident_Int (4);
      end L;

      generic
         S : Integer;
      function M return Integer;

      function M return Integer is
         T : Integer := M.S;
      begin
         if Ident_Int (M.T) /= 4 or M.S /= Ident_Int (4) then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
         end if;
         return Ident_Int (1);
      end M;

      function F1 is new M (4);

   begin
      if L (Int) /= 4 or F1 /= 1 then
         Failed ("INCORRECT RESULTS RETURNED FROM FUNCTION - 9");
      end if;
   end;
   -------------------------------------------------------------------

   declare
      task L is
         entry E (A : Integer);
      end L;

      task type M is
         entry E1 (A : Integer);
      end M;

      T1 : M;

      task body L is
         X : Integer := Ident_Int (1);
         R : Integer renames X;
         Y : Integer := L.R;
      begin
         X := X + L.R;
         if X /= Ident_Int (2) or Y /= Ident_Int (1) then
            Failed ("INCORRECT RESULTS FROM EXPANDED NAME - " & "10");
         end if;
      end L;

      task body M is
         X : Integer := Ident_Int (2);
         R : Integer renames X;
         Y : Integer := M.R;
      begin
         accept E1 (A : Integer) do
            X := X + M.R;
            if X /= Ident_Int (4) or Y /= Ident_Int (2) then
               Failed ("INCORRECT RESULTS FROM EXPANDED " & "NAME - 11");
            end if;
            if E1.A /= Ident_Int (3) then
               Failed ("INCORRECT RESULTS FROM EXPANDED " & "NAME - 12");
            end if;
         end E1;
      end M;
   begin
      T1.E1 (3);
   end;
   -------------------------------------------------------------------

   declare
      task T is
         entry G (1 .. 2) (A : Integer);
      end T;

      task body T is
      begin
         accept G (1) (A : Integer) do
            if G.A /= Ident_Int (2) then
               Failed ("INCORRECT RESULTS FROM EXPANDED " & "NAME - 13");
            end if;
            Blk :
            declare
               B : Integer := 7;
            begin
               if T.Blk.B /= Ident_Int (7) then
                  Failed ("INCORRECT RESULTS FROM " & "EXPANDED NAME - 14");
               end if;
            end Blk;
         end G;
         accept G (2) (A : Integer) do
            if G.A /= Ident_Int (1) then
               Failed ("INCORRECT RESULTS FROM EXPANDED " & "NAME - 15");
            end if;
         end G;
      end T;
   begin
      T.G (1) (2);
      T.G (2) (1);
   end;
   -------------------------------------------------------------------

   Swap :
   declare
      Var        : Character := '*';
      Rename_Var : Character renames Var;
      New_Var    : Character;
   begin
      if Equal (3, 3) then
         New_Var := Swap.Rename_Var;
      end if;
      if New_Var /= Ident_Char ('*') then
         Failed ("INCORRECT RESULTS FROM EXPANDED NAME - " & "16");
      end if;
      Lp :
      for I in 1 .. 2 loop
         if Swap.Lp.I = Ident_Int (2) or Lp.I = Ident_Int (1) then
            goto Swap.Lab1;
         end if;
         New_Var := Ident_Char ('+');
         <<Lab1>>
         New_Var := Ident_Char ('-');
      end loop Lp;
      if New_Var /= Ident_Char ('-') then
         Failed ("INCORRECT RESULTS FROM FOR LOOP - 17");
      end if;
   end Swap;

   Result;
end C41307d;

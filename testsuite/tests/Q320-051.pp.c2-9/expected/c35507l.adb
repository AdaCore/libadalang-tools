-- C35507L.ADA

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
-- CHECK THAT THE ATTRIBUTES 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE
-- PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A CHARACTER TYPE.

-- RJW 6/03/86
-- PWN 11/30/94 REMOVED TESTS BASED ON 128 CHARACTERS FOR ADA 9X.

with Report; use Report;

procedure C35507l is

   type Char is ('A', B);

   type Newchar is new Char;

begin

   Test
     ("C35507L",
      "CHECK THAT THE ATTRIBUTES 'POS' AND " &
      "'VAL' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL PARAMETER IS A CHARACTER TYPE");

   declare
      generic
         type Chtype is (<>);
         Str : String;
         I1 : Integer;
      procedure P;

      procedure P is
         subtype Subch is Chtype;
         Ch       : Chtype;
         Position : Integer;
      begin
         Position := 0;
         for Ch in Chtype loop
            if Subch'Pos (Ch) /= Position then
               Failed
                 ("INCORRECT VALUE FOR " &
                  Str &
                  "'POS OF " &
                  Chtype'Image (Ch));
            end if;

            if Subch'Val (Position) /= Ch then
               Failed
                 ("INCORRECT VALUE FOR " &
                  Str &
                  "'VAL OF CHARACTER IN POSITION - " &
                  Integer'Image (Position));
            end if;
            Position := Position + 1;
         end loop;

         begin
            if Subch'Val (-1) = Subch'Val (0) then
               Failed
                 ("NO EXCEPTION RAISED " & "FOR " & Str & "'VAL (-1) - 1");
            else
               Failed
                 ("NO EXCEPTION RAISED " & "FOR " & Str & "'VAL (-1) - 2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED " & "FOR " & Str & "'VAL (-1)");
         end;
      end P;

      procedure Pchar is new P (Char, "CHAR", 1);
      procedure Pnchar is new P (Newchar, "NEWCHAR", 1);
      procedure Pch is new P (Character, "CHARACTER", 127);
   begin
      Pchar;
      Pnchar;
      Pch;
   end;

   Result;
end C35507l;

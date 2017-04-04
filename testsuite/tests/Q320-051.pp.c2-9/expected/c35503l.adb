-- C35503L.ADA

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
--     PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER
--     IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35503l is

begin
   Test
     ("C35503L",
      "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "GENERIC FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL PARAMETER IS AN INTEGER TYPE");

   declare
      type Intrange is range -6 .. 6;

      generic
         type Int is (<>);
      procedure P (Str : String);

      procedure P (Str : String) is
         subtype Sint is
           Int range Int'Val (Ident_Int (-4)) .. Int'Val (Ident_Int (4));
         I : Integer;
      begin
         I := Ident_Int (-6);
         for S in Int'Val (Ident_Int (-6)) .. Int'Val (Ident_Int (6)) loop
            begin
               if Sint'Pos (S) /= I then
                  Failed
                    ("WRONG VALUE FOR " & Str & "'POS OF " & Int'Image (S));
               end if;
            exception
               when others =>
                  Failed
                    ("EXCEPTION RAISED FOR " &
                     Str &
                     "'POS " &
                     "OF " &
                     Int'Image (S));
            end;
            begin
               if Sint'Val (I) /= S then
                  Failed
                    ("WRONG VALUE FOR " &
                     Str &
                     "'VAL " &
                     "OF " &
                     Int'Image (S));
               end if;
            exception
               when others =>
                  Failed
                    ("EXCEPTION RAISED FOR " &
                     Str &
                     "'VAL " &
                     "OF " &
                     Int'Image (S));
            end;
            I := I + 1;
         end loop;
      end P;

      procedure P1 is new P (Intrange);
      procedure P2 is new P (Integer);

   begin
      P1 ("INTRANGE");
      P2 ("INTEGER");
   end;

   Result;

end C35503l;

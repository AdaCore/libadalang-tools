-- C35507H.ADA

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
-- CHECK THAT THE ATTRIBUTES 'PRED' AND 'SUCC' YIELD THE CORRECT
-- RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL
-- PARAMETER IS A CHARACTER TYPE.

-- RJW 6/03/86
-- DWC 7/01/87     -- ADDED THIRD VALUE TO CHAR TYPE.
-- REMOVED SECTION OF CODE AND PLACED INTO
-- C35505E.ADA.

with Report; use Report;

procedure C35507h is

   type Char is ('A', B, C);

   type Newchar is new Char;

begin

   Test
     ("C35507H",
      "CHECK THAT THE ATTRIBUTES 'PRED' AND " &
      "'SUCC' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL PARAMETER IS A CHARACTER TYPE");

   declare
      generic
         type Chtype is (<>);
         Str : String;
         I1, I2 : Integer;
      procedure P;

      procedure P is
         subtype Subch is Chtype range Chtype'Val (I1) .. Chtype'Val (I2);

      begin
         for Ch in Subch'Val (I1 + 1) .. Subch'Val (I2) loop
            if Subch'Pred (Ch) /= Subch'Val (Subch'Pos (Ch) - 1) then
               Failed
                 ("INCORRECT VALUE FOR " &
                  Str &
                  "'PRED OF " &
                  Subch'Image (Ch));
            end if;
         end loop;

         for Ch in Subch'Val (I1) .. Subch'Val (I2 - 1) loop
            if Subch'Succ (Ch) /= Subch'Val (Subch'Pos (Ch) + 1) then
               Failed
                 ("INCORRECT VALUE FOR " &
                  Str &
                  "'SUCC OF " &
                  Subch'Image (Ch));
            end if;
         end loop;

      end P;

      procedure Pchar is new P (Char, "CHAR", 0, 1);
      procedure Pnchar is new P (Newchar, "NEWCHAR", 0, 1);
      procedure Pch is new P (Character, "CHARACTER", 0, 127);
   begin
      Pchar;
      Pnchar;
      Pch;
   end;

   Result;
end C35507h;

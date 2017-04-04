-- C35507P.ADA

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
-- CHECK THAT THE ATTRIBUTES 'FIRST' AND 'LAST' YIELD THE CORRECT RESULTS WHEN
-- THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A CHARACTER
-- TYPE.

-- RJW 6/03/86
-- PWN 11/30/94 REMOVED TESTS BASED ON 128 CHARACTERS FOR ADA 9X.

with Report; use Report;

procedure C35507p is

   type Char is ('A', B);

   type Newchar is new Char;

   Space : constant Character := ' ';

   subtype Graphic is Character range Space .. Ascii.Tilde;
   subtype Nongraphic is Character range Ascii.Nul .. Ascii.Us;
begin

   Test
     ("C35507P",
      "CHECK THAT THE ATTRIBUTES 'FIRST' AND " &
      "'LAST' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL PARAMETER IS A CHARACTER TYPE");

   declare
      generic
         type Chtype is (<>);
         Str : String;
         F, L : Chtype;
      procedure P;

      procedure P is
         subtype Nochar is Chtype range L .. F;
      begin
         if Chtype'First /= F then
            Failed ("INCORRECT VALUE FOR " & Str & "'FIRST");
         end if;

         if Chtype'Last /= L then
            Failed ("INCORRECT VALUE FOR " & Str & "'LAST");
         end if;

         if Nochar'First /= L then
            Failed
              ("INCORRECT VALUE FOR NOCHAR'FIRST AS A " & "SUBTYPE OF " & Str);
         end if;

         if Nochar'Last /= F then
            Failed
              ("INCORRECT VALUE FOR NOCHAR'LAST AS A " & "SUBTYPE OF " & Str);
         end if;
      end P;

      procedure P1 is new P (Char, "CHAR", 'A', B);
      procedure P2 is new P (Newchar, "NEWCHAR", 'A', B);
      procedure P3 is new P (Graphic, "GRAPHIC", Space, Ascii.Tilde);
      procedure P4 is new P (Nongraphic, "NONGRAPHIC", Ascii.Nul, Ascii.Us);
   begin
      P1;
      P2;
      P3;
      P4;
   end;

   Result;
end C35507p;

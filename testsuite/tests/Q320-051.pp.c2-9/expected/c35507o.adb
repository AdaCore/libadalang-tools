-- C35507O.ADA

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
-- CHECK THAT THE ATTRIBUTES 'FIRST' AND 'LAST' YIELD THE CORRECT
-- RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.

-- RJW 6/03/86
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.
--              REMOVED PART OF TEST INVALID FOR ADA 9X.

with Report; use Report;

procedure C35507o is

   type Char is ('A', B);

   type Newchar is new Char;

   Space : constant Character := Character'(' ');

   subtype Nochar is Character range Character'('Z') .. Character'('A');
   subtype Graphic is Character range Space .. Ascii.Tilde;
   subtype Nongraphic is Character range Ascii.Nul .. Ascii.Us;

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
     ("C35507O",
      "CHECK THAT THE ATTRIBUTES 'FIRST' AND " &
      "'LAST' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A CHARACTER TYPE");

   begin
      if Ident (Char'First) /= 'A' then
         Failed ("INCORRECT VALUE FOR CHAR'FIRST");
      end if;

      if Char'Last /= B then
         Failed ("INCORRECT VALUE FOR CHAR'LAST");
      end if;
   end;

   begin
      if Newchar'First /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'FIRST");
      end if;

      if Newchar'Last /= Ident (B) then
         Failed ("INCORRECT VALUE FOR NEWCHAR'LAST");
      end if;
   end;

   begin
      if Nochar'First /= Character'('Z') then
         Failed ("INCORRECT VALUE FOR NOCHAR'FIRST");
      end if;

      if Nochar'Last /= Character'('A') then
         Failed ("INCORRECT VALUE FOR NOCHAR'LAST");
      end if;
   end;

   begin
      if Character'First /= Ascii.Nul then
         Failed ("INCORRECT VALUE FOR CHARACTER'FIRST");
      end if;

   end;

   begin
      if Nongraphic'First /= Ident_Char (Ascii.Nul) then
         Failed ("INCORRECT VALUE FOR NONGRAPHIC'FIRST");
      end if;

      if Nongraphic'Last /= Ascii.Us then
         Failed ("INCORRECT VALUE FOR NONGRAPHIC'LAST");
      end if;
   end;

   begin
      if Graphic'First /= Space then
         Failed ("INCORRECT VALUE FOR GRAPHIC'FIRST");
      end if;

      if Graphic'Last /= Ascii.Tilde then
         Failed ("INCORRECT VALUE FOR GRAPHIC'LAST");
      end if;
   end;

   Result;
end C35507o;

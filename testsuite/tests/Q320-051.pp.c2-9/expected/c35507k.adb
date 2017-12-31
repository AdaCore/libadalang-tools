-- C35507K.ADA

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
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.

-- HISTORY:
--     RJW 06/03/86
--     JLH 07/28/87  MODIFIED FUNCTION IDENT.
-- PWN 11/30/94 REMOVED PART OF TEST INVALID FOR ADA 9X.

with Report; use Report;

procedure C35507k is

   type Char is ('A', B);

   type Newchar is new Char;

   subtype Schar is Character range Character'Val (127) .. Character'Val (127);

   Blank : constant Character := ' ';

   Position : Integer;

   Nongraph : array (0 .. 31) of Character :=
     (Ascii.Nul, Ascii.Soh, Ascii.Stx, Ascii.Etx, Ascii.Eot, Ascii.Enq,
      Ascii.Ack, Ascii.Bel, Ascii.Bs, Ascii.Ht, Ascii.Lf, Ascii.Vt, Ascii.Ff,
      Ascii.Cr, Ascii.So, Ascii.Si, Ascii.Dle, Ascii.Dc1, Ascii.Dc2, Ascii.Dc3,
      Ascii.Dc4, Ascii.Nak, Ascii.Syn, Ascii.Etb, Ascii.Can, Ascii.Em,
      Ascii.Sub, Ascii.Esc, Ascii.Fs, Ascii.Gs, Ascii.Rs, Ascii.Us);

   function Ident (Ch : Char) return Char is
   begin
      if Equal (Char'Pos (Ch), Char'Pos (Ch)) then
         return Ch;
      end if;
      return Char'First;
   end Ident;

   function Ident (Ch : Newchar) return Newchar is
   begin
      if Equal (Newchar'Pos (Ch), Newchar'Pos (Ch)) then
         return Ch;
      end if;
      return Newchar'First;
   end Ident;

begin

   Test
     ("C35507K",
      "CHECK THAT THE ATTRIBUTES 'POS' AND " &
      "'VAL' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A CHARACTER TYPE");

   begin
      if Char'Pos ('A') /= 0 then
         Failed ("INCORRECT VALUE FOR CHAR'POS('A') - 1");
      end if;

      if Char'Pos (B) /= 1 then
         Failed ("INCORRECT VALUE FOR CHAR'POS(B) - 1");
      end if;

      if Char'Val (0) /= 'A' then
         Failed ("INCORRECT VALUE FOR CHAR'VAL(0)");
      end if;

      if Char'Val (1) /= B then
         Failed ("INCORRECT VALUE FOR CHAR'VAL(1)");
      end if;

      if Char'Pos (Ident ('A')) /= 0 then
         Failed ("INCORRECT VALUE " & "FOR CHAR'POS (IDENT ('A')) - 2");
      end if;

      if Char'Pos (Ident (B)) /= 1 then
         Failed ("INCORRECT VALUE " & "FOR CHAR'POS (IDENT (B)) - 2");
      end if;

   end;

   begin
      if Newchar'Pos ('A') /= 0 then
         Failed ("INCORRECT VALUE FOR NEWCHAR'POS('A')");
      end if;

      if Newchar'Pos (B) /= 1 then
         Failed ("INCORRECT VALUE FOR NEWCHAR'POS(B) - 1");
      end if;

      if Newchar'Val (0) /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'VAL(0) - 1");
      end if;

      if Newchar'Val (1) /= B then
         Failed ("INCORRECT VALUE FOR NEWCHAR'VAL(1)");
      end if;

      if Newchar'Val (Ident_Int (1)) /= B then
         Failed ("INCORRECT VALUE " & "FOR NEWCHAR'POS (IDENT (B)) - 2");
      end if;

      if (Newchar'Val (Ident_Int (0))) /= 'A' then
         Failed ("INCORRECT VALUE " & "FOR IDENT (NEWCHAR'VAL (0)) - 2");
      end if;

   end;

   begin
      if Char'Val (Ident_Int (2)) = B then
         Failed ("NO EXCEPTION RAISED " & "FOR CHAR'VAL (IDENT_INT (2)) - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "FOR CHAR'VAL (IDENT_INT (2)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR CHAR'VAL (IDENT_INT (2))");
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

   Position := 0;

   for Ch in Character loop
      if Schar'Pos (Ch) /= Position then
         Failed ("INCORRECT VALUE FOR SCHAR'POS OF " & Character'Image (Ch));
      end if;

      Position := Position + 1;
   end loop;

   for Position in 0 .. 31 loop
      if Character'Val (Position) /= Nongraph (Position) then
         Failed
           ("INCORRECT VALUE FOR CHARACTER'VAL OF " &
            "NONGRAPHIC CHARACTER IN POSITION - " & Integer'Image (Position));
      end if;
   end loop;

   Position := 32;

   for Ch in Blank .. Ascii.Tilde loop
      if Schar'Val (Position) /= Ch then
         Failed
           ("INCORRECT VALUE FOR SCHAR'VAL OF " &
            "GRAPHIC CHARACTER IN POSITION - " & Integer'Image (Position));
      end if;

      Position := Position + 1;
   end loop;

   if Character'Val (127) /= Ascii.Del then
      Failed
        ("INCORRECT VALUE FOR CHARACTER'VAL OF " &
         "NONGRAPHIC CHARACTER IN POSITION - 127");
   end if;

   begin
      if Character'Val (Ident_Int (-1)) = Ascii.Nul then
         Failed
           ("NO EXCEPTION RAISED " & "FOR CHARACTER'VAL (IDENT_INT (-1)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED " & "FOR CHARACTER'VAL (IDENT_INT (-1)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " & "FOR CHARACTER'VAL (IDENT_INT (-1))");
   end;

   Result;
end C35507k;

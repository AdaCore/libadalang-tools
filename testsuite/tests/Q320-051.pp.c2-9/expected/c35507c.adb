-- C35507C.ADA

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
--     CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.
--     SUBTESTS ARE:
--         (A). TESTS FOR IMAGE.
--         (B). TESTS FOR VALUE.

-- HISTORY:
--     RJW 05/29/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.
--                   CORRECTED ERROR MESSAGES AND ADDED CALLS TO
--                   IDENT_STR.

with Report; use Report;

procedure C35507c is

   type Char is ('A', 'a');

   type Newchar is new Char;

   function Ident (Ch : Char) return Char is
   begin
      return Char'Val (Ident_Int (Char'Pos (Ch)));
   end Ident;

   function Ident (Ch : Newchar) return Newchar is
   begin
      return Newchar'Val (Ident_Int (Newchar'Pos (Ch)));
   end Ident;

   procedure Check_Bound (Str1, Str2 : String) is
   begin
      if Str1'First /= 1 then
         Failed
           ("INCORRECT LOWER BOUND FOR " & Str2 & "'IMAGE ('" & Str1 & "')");
      end if;
   end Check_Bound;

begin

   Test
     ("C35507C",
      "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
      "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A CHARACTER TYPE");

   begin  -- (A).
      if Char'Image ('A') /= "'A'" then
         Failed ("INCORRECT IMAGE FOR CHAR'('A')");
      end if;

      Check_Bound (Char'Image ('A'), "CHAR");

      if Char'Image ('a') /= "'a'" then
         Failed ("INCORRECT IMAGE FOR CHAR'('a')");
      end if;

      Check_Bound (Char'Image ('a'), "CHAR");

      if Newchar'Image ('A') /= "'A'" then
         Failed ("INCORRECT IMAGE FOR NEWCHAR'('A')");
      end if;

      Check_Bound (Newchar'Image ('A'), "NEWCHAR");

      if Newchar'Image ('a') /= "'a'" then
         Failed ("INCORRECT IMAGE FOR NEWCHAR'('a')");
      end if;

      Check_Bound (Newchar'Image ('a'), "NEWCHAR");

      if Char'Image (Ident ('A')) /= "'A'" then
         Failed ("INCORRECT IMAGE FOR CHAR'( IDENT ('A'))");
      end if;

      Check_Bound (Char'Image (Ident ('A')), "IDENT OF CHAR");

      if Char'Image (Ident ('a')) /= "'a'" then
         Failed ("INCORRECT IMAGE FOR CHAR'( IDENT ('a'))");
      end if;

      Check_Bound (Char'Image (Ident ('a')), "IDENT OF CHAR");

      if Newchar'Image (Ident ('A')) /= "'A'" then
         Failed ("INCORRECT IMAGE FOR NEWCHAR'( IDENT ('A'))");
      end if;

      Check_Bound (Newchar'Image (Ident ('A')), "IDENT OF NEWCHAR");

      if Newchar'Image (Ident ('a')) /= "'a'" then
         Failed ("INCORRECT IMAGE FOR NEWCHAR'( IDENT ('a'))");
      end if;

      Check_Bound (Newchar'Image (Ident ('a')), "IDENT OF NEWCHAR");

      for Ch in Character'Val (32) .. Character'Val (126) loop
         if Character'Image (Ch) /= ("'" & Ch) & "'" then
            Failed ("INCORRECT IMAGE FOR CHARACTER'(" & Ch & ")");
         end if;

         Check_Bound (Character'Image (Ch), "CHARACTER");

      end loop;

      for Ch in Character'Val (0) .. Character'Val (31) loop
         Check_Bound (Character'Image (Ch), "CHARACTER");
      end loop;

      Check_Bound (Character'Image (Character'Val (127)), "CHARACTER");

   end;

   ---------------------------------------------------------------

   declare -- (B).

      subtype Subchar is
        Character range Character'Val (127) .. Character'Val (127);
   begin
      for Ch in Character'Val (32) .. Character'Val (126) loop
         if Subchar'Value (("'" & Ch) & "'") /= Ch then
            Failed ("INCORRECT SUBCHAR'VALUE FOR " & Ch);
         end if;
      end loop;

      for Ch in Character'Val (0) .. Character'Val (31) loop
         if Subchar'Value (Character'Image (Ch)) /= Ch then
            Failed ("INCORRECT SUBCHAR'VALUE FOR " & Character'Image (Ch));
         end if;
      end loop;

      if Subchar'Value (Character'Image (Character'Val (127))) /=
        Character'Val (127) then
         Failed ("INCORRECT SUBCHAR'VALUE FOR " & "CHARACTER'VAL (127)");
      end if;
   end;

   begin
      if Char'Value ("'A'") /= 'A' then
         Failed ("INCORRECT VALUE FOR CHAR'(""'A'"")");
      end if;

      if Char'Value ("'a'") /= 'a' then
         Failed ("INCORRECT VALUE FOR CHAR'(""'a'"")");
      end if;

      if Newchar'Value ("'A'") /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'(""'A'"")");
      end if;

      if Newchar'Value ("'a'") /= 'a' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'(""'a'"")");
      end if;
   end;

   begin
      if Char'Value (Ident_Str ("'A'")) /= 'A' then
         Failed ("INCORRECT VALUE FOR CHAR'(IDENT_STR" & "(""'A'""))");
      end if;

      if Char'Value (Ident_Str ("'a'")) /= 'a' then
         Failed ("INCORRECT VALUE FOR CHAR'(IDENT_STR" & "(""'a'""))");
      end if;

      if Newchar'Value (Ident_Str ("'A'")) /= 'A' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'(IDENT_STR" & "(""'A'""))");
      end if;

      if Newchar'Value (Ident_Str ("'a'")) /= 'a' then
         Failed ("INCORRECT VALUE FOR NEWCHAR'(IDENT_STR" & "(""'a'""))");
      end if;
   end;

   begin
      if Char'Value (Ident_Str ("'B'")) = 'A' then
         Failed
           ("NO EXCEPTION RAISED " &
            "FOR CHAR'VALUE (IDENT_STR (""'B'"")) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED " &
            "FOR CHAR'VALUE (IDENT_STR (""'B'"")) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHAR'VALUE (IDENT_STR (""'B'""))");
   end;

   begin
      if Character'Value (Ident_Char (Ascii.Ht) & "'A'") = 'A' then
         Failed
           ("NO EXCEPTION RAISED FOR " & "CHARACTER'VALUE " &
            "(IDENT_CHAR (ASCII.HT) & ""'A'"") - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " & "CHARACTER'VALUE " &
            "(IDENT_CHAR (ASCII.HT) & ""'A'"") - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " & "FOR CHARACTER'VALUE " &
            "(IDENT_CHAR (ASCII.HT) & ""'A'"")");
   end;

   begin
      if Character'Value ("'B'" & Ident_Char (Ascii.Ht)) = 'B' then
         Failed
           ("NO EXCEPTION RAISED FOR " & "CHARACTER'VALUE (""'B'"" & " &
            "IDENT_CHAR (ASCII.HT)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " & "CHARACTER'VALUE (""'B'"" & " &
            "IDENT_CHAR (ASCII.HT)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " & "FOR CHARACTER'VALUE (""'B'"" & " &
            "IDENT_CHAR (ASCII.HT)) ");
   end;

   begin
      if Character'Value ("'C'" & Ident_Char (Ascii.Bel)) = 'C' then
         Failed
           ("NO EXCEPTION RAISED FOR " & "CHARACTER'VALUE (""'C'"" & " &
            "IDENT_CHAR (ASCII.BEL)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " & "CHARACTER'VALUE (""'C'"" & " &
            "IDENT_CHAR (ASCII.BEL)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " & "FOR CHARACTER'VALUE (""'C'"" & " &
            "IDENT_CHAR (ASCII.BEL))");
   end;

   begin
      if Character'Value (Ident_Str ("'")) = ''' then
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""'"")) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""'"")) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'VALUE (IDENT_STR (""'""))");
   end;

   begin
      if Character'Value (Ident_Str ("''")) = ''' then
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""''"")) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""''"")) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'VALUE (IDENT_STR (""''""))");
   end;

   begin
      if Character'Value (Ident_Str ("'A")) = 'A' then
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""'A"")) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""'A"")) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'VALUE IDENT_STR (""'A""))");
   end;

   begin
      if Character'Value (Ident_Str ("A'")) = 'A' then
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""A'"")) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""A'"")) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'VALUE (IDENT_STR (""A'""))");
   end;

   begin
      if Character'Value (Ident_Str ("'AB'")) = 'A' then
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""'AB'"")) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " &
            "CHARACTER'VALUE (IDENT_STR (""'AB'"")) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED " &
            "FOR CHARACTER'VALUE IDENT_STR (""'AB'""))");
   end;

   Result;
end C35507c;

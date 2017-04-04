-- C35508C.ADA

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
-- CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULTS WHEN
-- THE PREFIX IS A BOOLEAN TYPE.

-- SUBTESTS ARE:
--     (A). TESTS FOR IMAGE.
--     (B). TESTS FOR VALUE.

-- RJW 3/19/86

with Report; use Report;

procedure C35508c is

   type Newbool is new Boolean;

begin

   Test
     ("C35508C",
      "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
      "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A BOOLEAN TYPE");
-- PART (A).

   declare

      A5, B5 : Integer := Ident_Int (5);
      C6     : Integer := Ident_Int (6);
   begin

      if Boolean'Image (A5 = B5) /= "TRUE" then
         Failed ("INCORRECT IMAGE FOR 'A5 = B5'");
      end if;
      if Boolean'Image (A5 = B5)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR 'A5 = B5'");
      end if;

      if Boolean'Image (C6 = A5) /= "FALSE" then
         Failed ("INCORRECT IMAGE FOR 'C6 = A5'");
      end if;
      if Boolean'Image (C6 = A5)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR 'C6 = A5'");
      end if;

      if Boolean'Image (True) /= "TRUE" then
         Failed ("INCORRECT IMAGE FOR 'TRUE'");
      end if;
      if Boolean'Image (True)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR 'TRUE'");
      end if;

      if Newbool'Image (False) /= "FALSE" then
         Failed ("INCORRECT IMAGE FOR NEWBOOL'FALSE'");
      end if;
      if Newbool'Image (False)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR NEWBOOL'FALSE'");
      end if;
   end;

-----------------------------------------------------------------------

-- PART (B).

   begin
      if Boolean'Value (Ident_Str ("TRUE")) /= True then
         Failed ("INCORRECT VALUE FOR ""TRUE""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""TRUE""");
   end;

   begin
      if Newbool'Value (Ident_Str ("FALSE")) /= False then
         Failed ("INCORRECT VALUE FOR ""FALSE""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""FALSE""");
   end;

   begin
      if Boolean'Value ("true") /= True then
         Failed ("INCORRECT VALUE FOR ""true""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""true""");
   end;

   begin
      if Newbool'Value ("false") /= False then
         Failed ("INCORRECT VALUE FOR ""false""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR " & """false""");
   end;

   begin
      if Boolean'Value (Ident_Str ("TRUE     ")) /= True then
         Failed ("INCORRECT VALUE WITH TRAILING BLANKS");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE - " & "TRAILING BLANKS");
   end;

   begin
      if Newbool'Value ("  FALSE") /= False then
         Failed ("INCORRECT VALUE WITH LEADING BLANKS");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE - LEADING " & "BLANKS");
   end;

   declare
      subtype Subbool is Boolean range False .. False;
   begin
      if Subbool'Value (Ident_Str ("TRUE")) /= True then
         Failed ("INCORRECT VALUE - ""TRUE"" AND " & "SUBBOOL");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - SUBBOOL");
   end;

   begin
      if Boolean'Value (Ident_Str ("MAYBE")) = True then
         Failed ("NO EXCEPTION RAISED - ""MAYBE"" - 1");
      else
         Failed ("NO EXCEPTION RAISED - ""MAYBE"" - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ""MAYBE"" ");
   end;

   begin
      if Boolean'Value (Ident_Char (Ascii.Ht) & "TRUE") = True then
         Failed ("NO EXCEPTION RAISED - LEADING 'HT' - 1");
      else
         Failed ("NO EXCEPTION RAISED - LEADING 'HT' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - LEADING 'HT'");
   end;

   begin
      if Newbool'Value ("FALSE" & Ascii.Ht) = False then
         Failed ("NO EXCEPTION RAISED - TRAILING 'HT' - 1");
      else
         Failed ("NO EXCEPTION RAISED - TRAILING 'HT' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - TRAILING 'HT'");
   end;

   Result;
end C35508c;

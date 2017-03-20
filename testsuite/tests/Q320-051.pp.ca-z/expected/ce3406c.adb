-- CE3406C.ADA

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
--     CHECK THAT SKIP_PAGE RAISES END_ERROR WHEN THE FILE IS POSITIONED
--     BEFORE THE FILE TERMINATOR BUT NOT WHEN THE FILE IS POSITIONED
--     BEFORE THE FINAL PAGE TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     JBG 01/24/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/24/87  CREATED NON-TEMPORARY FILE, REMOVED DEPENDENCE
--                   ON RESET, AND CHECKED CHARACTER READ IN.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3406c is

   Incomplete : exception;
   File      : File_Type;
   Char      : Character      := ('C');
   Item_Char : Character;
   Two       : Positive_Count := Positive_Count (Ident_Int (2));

begin

   Test
     ("CE3406C",
      "CHECK THAT SKIP_PAGE RAISES END_ERROR WHEN " &
      "THE FILE IS POSITIONED BEFORE THE FILE " &
      "TERMINATOR BUT NOT WHEN THE FILE IS " &
      "POSITIONED BEFORE THE FINAL PAGE TERMINATOR");

-- CREATE AND INITIALIZE FILE

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   for I in 1 .. 2 loop
      for I in 1 .. 3 loop
         Put (File, Char);
      end loop;
      New_Line (File);
   end loop;

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

-- START TEST

-- TEST SKIP_PAGE BEFORE FINAL PAGE TERMINATOR

   while not End_Of_Page (File) loop
      Get (File, Item_Char);
      if Item_Char /= 'C' then
         Failed ("INCORRECT VALUE READ FROM FILE");
      end if;
   end loop;

   begin
      Skip_Page (File);
   exception
      when End_Error =>
         Failed ("RAISED END_ERROR BEFORE FINAL PAGE " & "TERMINATOR - 1");
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 1");
   end;

   if Page (File) /= Two then
      Failed ("PAGE NOT SET TO TWO");
   end if;

-- TEST SKIP_PAGE BEFORE FILE TERMINATOR
   begin
      Skip_Page (File);
      Failed ("END_ERROR NOT RAISED");
   exception
      when End_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 2");
   end;

   begin
      Delete (File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3406c;

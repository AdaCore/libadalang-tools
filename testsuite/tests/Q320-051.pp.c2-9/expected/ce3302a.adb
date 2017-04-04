-- CE3302A.ADA

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
--     CHECK THAT SET_LINE_LENGTH, SET_PAGE_LENGTH, LINE_LENGTH, AND
--     PAGE_LENGTH RAISE MODE_ERROR WHEN APPLIED TO A FILE OF MODE
--     IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/19/87  CREATED AN EXTERNAL FILE WITH A NAME, REMOVED
--                   DEPENDENCE ON RESET, AND ADDED CODE TO DELETE
--                   EXTERNAL FILE.
with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3302a is

   Incomplete : exception;
   File : File_Type;
   Five : Count     := Count (Ident_Int (5));
   Var1 : Count;
   Item : Character := 'A';

begin
   Test
     ("CE3302A",
      "CHECK THAT SET_LINE_LENGTH, SET_PAGE_LENGTH, " &
      "LINE_LENGTH, AND PAGE_LENGTH RAISE MODE_ERROR " &
      "WHEN APPLIED TO A FILE OF MODE IN_FILE");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON TEXT FILE CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON TEXT FILE CREATE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR TEXT CREATE");
         raise Incomplete;
   end;

   Put (File, Item);
   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON TEXT FILE OPEN");
         raise Incomplete;
   end;

   begin
      Set_Line_Length (File, Five);
      Failed ("MODE_ERROR NOT RAISED - SET_LINE_LENGTH");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - SET_LINE_LENGTH");
   end;

   begin
      Set_Page_Length (File, Five);
      Failed ("MODE_ERROR NOT RAISED - SET_PAGE_LENGTH");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - SET_PAGE_LENGTH");
   end;

   begin
      Var1 := Line_Length (File);
      Failed ("MODE_ERROR NOT RAISED - LINE_LENGTH");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - LINE_LENGTH");
   end;

   begin
      Var1 := Page_Length (File);
      Failed ("MODE_ERROR NOT RAISED - PAGE_LENGTH");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - PAGE_LENGTH");
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

end Ce3302a;

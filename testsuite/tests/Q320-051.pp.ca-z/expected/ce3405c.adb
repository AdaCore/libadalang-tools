-- CE3405C.ADA

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
--     CHECK THAT NEW_PAGE RAISES MODE_ERROR IF THE FILE SPECIFIED
--     HAS MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/23/87  CREATED AN EXTERNAL FILE, REMOVED DEPENDENCE ON
--                   RESET, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3405c is

   Incomplete : exception;
   File : File_Type;

begin

   Test
     ("CE3405C",
      "CHECK THAT NEW_PAGE RAISES MODE_ERROR IF THE " &
      "FILE SPECIFIED HAS MODE IN_FILE");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
         raise Incomplete;
   end;

   Put (File, "STUFF");

   Close (File);
   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED; TEXT OPEN " & "WITH IN_FILE MODE");
         raise Incomplete;
   end;

   begin
      New_Page (File);
      Failed ("MODE_ERROR NOT RAISED FOR IN_FILE");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR IN_FILE");
   end;

   begin
      New_Page (Standard_Input);
      Failed ("MODE_ERROR NOT RAISED FOR STANDARD_INPUT");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR STANDARD_INPUT");
   end;

   begin
      New_Page (Current_Input);
      Failed ("MODE_ERROR NOT RAISED FOR CURRENT_INPUT");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR CURRENT_INPUT");
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

end Ce3405c;

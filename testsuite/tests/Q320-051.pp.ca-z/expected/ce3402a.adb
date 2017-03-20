-- CE3402A.ADA

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
--     CHECK THAT NEW_LINE RAISES MODE_ERROR WHEN THE FILE MODE
--     IS IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/19/87  ADDED ATTEMPT TO DELETE THE FILE AND REPLACED
--                   RESET WITH CLOSE AND OPEN.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3402a is

   Incomplete : exception;
   File1 : File_Type;
   Spac  : constant Positive_Count := Positive_Count (Ident_Int (1));

begin

   Test
     ("CE3402A",
      "CHECK THAT NEW_LINE RAISES MODE_ERROR " &
      "WHEN THE FILE MODE IS IN_FILE");

   begin
      Create (File1, Out_File, Legal_File_Name);
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

   begin
      Put_Line (File1, "STUFF");
      Close (File1);
      Open (File1, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED FOR OPEN " & "WITH IN_FILE MODE");
         raise Incomplete;
   end;

   begin
      New_Line (File1, Spac);
      Failed ("MODE_ERROR NOT RAISED FOR IN_FILE");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR IN_FILE");
   end;

   begin
      New_Line (Standard_Input, Spac);
      Failed ("MODE_ERROR NOT RAISED FOR STANDARD_INPUT");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR STANDARD_INPUT");
   end;

   begin
      Delete (File1);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3402a;

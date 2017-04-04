-- CE3403A.ADA

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
--     CHECK THAT SKIP_LINE CAN ONLY BE APPLIED TO FILES OF MODE
--     IN_FILE, MODE_ERROR IS RAISED FOR FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT CREATION OF TEMPORARY FILES WITH OUT_FILE MODE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/04/87  REVISED EXCEPTION HANDLERS AND ADDED A CASE
--                   FOR STANDARD_OUTPUT.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3403a is

   Incomplete : exception;
   File : File_Type;
   Spac : Positive_Count := Positive_Count (Ident_Int (1));

begin

   Test
     ("CE3403A",
      "CHECK THAT SKIP_LINE CAN ONLY BE " &
      "APPLIED TO FILES OF MODE IN_FILE");

   begin
      Create (File, Out_File);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED; TEXT CREATE OF " &
            "TEMPORARY FILE WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
         raise Incomplete;
   end;

   begin
      Skip_Line (File, Spac);
      Failed ("MODE_ERROR NOT RAISED FOR OUT_FILE");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR OUT_FILE");
   end;

   begin
      Skip_Line (Current_Output, Spac);
      Failed ("MODE_ERROR NOT RAISED FOR CURRENT_OUTPUT");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR " & "CURRENT_OUTPUT");
   end;

   begin
      Skip_Line (Standard_Output, Spac);
      Failed ("MODE_ERROR NOT RAISED FOR STANDARD_OUTPUT");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR " & "STANDARD_OUTPUT");
   end;

   Close (File);

   Result;

exception
   when Incomplete =>
      Result;

end Ce3403a;

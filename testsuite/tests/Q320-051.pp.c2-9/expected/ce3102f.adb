-- CE3102F.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN AN EXTERNAL FILE
--     CANNOT BE RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES, BUT DO NOT SUPPORT RESET OF EXTERNAL FILES.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3102f is

   Incomplete : exception;
   File : File_Type;

begin

   Test
     ("CE3102F",
      "CHECK THAT USE_ERROR IS RAISED WHEN AN " &
      "EXTERNAL FILE CANNOT BE RESET");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
   end;

   begin
      Reset (File);
      Not_Applicable ("RESET FOR OUT_FILE MODE ALLOWED - 1");
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR RESET - 1");
   end;

   Put (File, "HELLO");

   begin
      Reset (File, In_File);
      Not_Applicable ("RESET FROM OUT_FILE TO IN_FILE MODE " & "ALLOWED - 1");
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RASIED FOR RESET - 2");
   end;

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("TEXT_IO NOT SUPPORTED FOR IN_FILE " & "OPEN");
         raise Incomplete;
   end;

   begin
      Reset (File);
      Not_Applicable ("RESET FOR IN_FILE MODE ALLOWED - 2");
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR RESET - 3");
   end;

   begin
      Reset (File, Out_File);
      Not_Applicable ("RESET FROM IN_FILE TO OUT_FILE MODE " & "ALLOWED - 2");
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR RESET - 4");
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

end Ce3102f;

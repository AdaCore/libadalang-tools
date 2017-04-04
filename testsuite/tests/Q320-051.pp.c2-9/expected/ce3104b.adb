-- CE3104B.ADA

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
--     CHECK THAT THE FILE REMAINS OPEN AFTER A RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     DWC 08/13/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3104b is

   Incomplete : exception;
   File  : File_Type;
   Item1 : String (1 .. 5) := "STUFF";

begin

   Test ("CE3104B", "CHECK THAT THE FILE REMAINS OPEN AFTER " & "A RESET");

   begin
      Create (File, Out_File, Legal_File_Name);
      Put_Line (File, Item1);
      Close (File);
   exception
      when Use_Error | Name_Error =>
         Not_Applicable ("CREATE WITH OUT_FILE MODE " & "NOT SUPPORTED");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED DURING " & "FILE I/O");
         raise Incomplete;
   end;

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("OPEN WITH IN_FILE MODE NOT " & "SUPPORTED");
         raise Incomplete;
   end;

   begin
      Reset (File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (File) then
      Close (File);
   else
      Failed ("RESET FOR IN_FILE, CLOSED FILE");
   end if;

   begin
      Open (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("OPEN WITH OUT_FILE MODE NOT " & "SUPPORTED");
         raise Incomplete;
   end;

   begin
      Reset (File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (File) then
      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;
   else
      Failed ("RESET FOR OUT_FILE CLOSED FILE");
   end if;

   Result;

exception
   when Incomplete =>
      Result;
end Ce3104b;

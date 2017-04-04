-- CE3104C.ADA

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
--     CHECK THAT THE MODE PARAMETER IN RESET CHANGES THE MODE OF A
--     GIVEN FILE, AND IF NO MODE IS SUPPLIED, THE MODE IS LEFT AS IT
--     WAS BEFORE THE RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     RESET FOR TEXT FILES.

-- HISTORY:
--     DWC 08/17/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3104c is

   Incomplete : exception;
   File   : File_Type;
   Item1  : String (1 .. 5) := "STUFF";
   Item2  : String (1 .. 5);
   Length : Natural;

begin

   Test ("CE3104C", "CHECK THAT THE FILE REMAINS OPEN AFTER " & "A RESET");

   begin
      Create (File, Out_File, Legal_File_Name);
      Put_Line (File, Item1);
   exception
      when Use_Error | Name_Error =>
         Not_Applicable ("CREATE WITH OUT_FILE MODE NOT " & "SUPPORTED");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED DURING " & "FILE I/O");
         raise Incomplete;
   end;

   begin
      Reset (File);
      if Mode (File) /= Out_File then
         Failed ("RESET CHANGED MODE OF OUT_FILE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET FOR OUT_FILE MODE NOT " & "SUPPORTED FOR TEXT FILES");
         raise Incomplete;
   end;

   begin
      Reset (File, In_File);
      if Mode (File) /= In_File then
         Failed ("RESET MODE TO IN_FILE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET FROM OUT_FILE TO IN_FILE " &
            "NOT SUPPORTED FOR TEXT FILES");
         raise Incomplete;
   end;

   begin
      Reset (File);
      if Mode (File) /= In_File then
         Failed ("RESET CHANGED MODE OF IN_FILE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET OF IN_FILE MODE NOT SUPPORTED " & "FOR TEXT FILES");
         raise Incomplete;
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
end Ce3104c;

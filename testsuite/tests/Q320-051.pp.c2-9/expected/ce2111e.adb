-- CE2111E.ADA

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

--     THIS OBJECTIVE IS BEING INTERPRETED AS : CHECK THAT A FILE
--     REMAINS OPEN AFTER AN ATTEMPT TO RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     DLD 08/13/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/28/85
--     JLH 07/23/87  REWROTE TEST ALGORITHM.

with Report; use Report;
with Direct_Io;

procedure Ce2111e is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;

   Dir_File : Dir_Io.File_Type;
   Var1     : Integer := 5;
   Incomplete : exception;

begin

   Test ("CE2111E", "CHECK THAT THE FILE REMAINS OPEN AFTER A RESET");

-- CREATE DIRECT TEST FILE

   begin
      Create (Dir_File, Out_File, Legal_File_Name);
      Write (Dir_File, Var1);
      Close (Dir_File);
   exception
      when Use_Error | Name_Error =>
         Not_Applicable ("DIRECT FILES NOT SUPPORTED");
         raise Incomplete;
   end;

-- OPEN FILE

   begin
      Open (Dir_File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH IN_FILE MODE NOT SUPPORTED " & "FOR DIR_IO");
         raise Incomplete;
   end;

-- RESET FILE

   begin
      Reset (Dir_File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (Dir_File) then
      Close (Dir_File);
   else
      Failed ("RESET FOR IN_FILE, CLOSED FILE");
   end if;

-- RE-OPEN AS OUT_FILE AND REPEAT TEST

   begin
      Open (Dir_File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH OUT_FILE MODE NOT " & "SUPPORTED FOR DIR_IO");
         raise Incomplete;
   end;

   begin
      Reset (Dir_File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (Dir_File) then
      Close (Dir_File);
   else
      Failed ("RESET FOR OUT_FILE, CLOSED FILE");
   end if;

-- RE-OPEN AS IN_OUT FILE AND REPEAT TEST

   begin
      Open (Dir_File, Inout_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH IN_OUT FILE MODE NOT " & "SUPPORTED FOR DIR_IO");
         raise Incomplete;
   end;

   begin
      Reset (Dir_File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (Dir_File) then
      begin
         Delete (Dir_File);
      exception
         when Use_Error =>
            null;
      end;
   else
      Failed ("RESET FOR INOUT_FILE, CLOSED FILE");
   end if;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2111e;

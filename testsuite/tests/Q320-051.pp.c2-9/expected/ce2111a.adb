-- CE2111A.ADA

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
--     SEQUENTIAL FILES.

-- HISTORY:
--     DLD 08/13/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/28/85
--     JLH 07/22/87  REWROTE TEST ALGORITHM.

with Report; use Report;
with Sequential_Io;

procedure Ce2111a is

   package Seq_Io is new Sequential_Io (Integer);
   use Seq_Io;

   Seq_File : Seq_Io.File_Type;
   Var1     : Integer := 5;
   Incomplete : exception;

begin

   Test ("CE2111A", "CHECK THAT THE FILE REMAINS OPEN AFTER A RESET");

-- CREATE SEQUENTIAL TEST FILE

   begin
      Create (Seq_File, Out_File, Legal_File_Name);
      Write (Seq_File, Var1);
      Close (Seq_File);
   exception
      when Use_Error | Name_Error =>
         Not_Applicable ("SEQUENTIAL FILES NOT SUPPORTED");
         raise Incomplete;
   end;

-- OPEN FILE

   begin
      Open (Seq_File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH IN_FILE MODE NOT SUPPORTED " & "FOR SEQ_IO");
         raise Incomplete;
   end;

-- RESET FILE

   begin
      Reset (Seq_File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (Seq_File) then
      Close (Seq_File);
   else
      Failed ("RESET FOR IN_FILE, CLOSED FILE");
   end if;

-- RE-OPEN AS OUT_FILE AND REPEAT TEST

   begin
      Open (Seq_File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("OPEN WITH OUT_FILE MODE NOT " & "SUPPORTED FOR SEQ_IO");
         raise Incomplete;
   end;

   begin
      Reset (Seq_File);
   exception
      when Use_Error =>
         null;
   end;

   if Is_Open (Seq_File) then
      begin
         Delete (Seq_File);
      exception
         when Use_Error =>
            null;
      end;
   else
      Failed ("RESET FOR OUT_FILE, CLOSED FILE");
   end if;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2111a;

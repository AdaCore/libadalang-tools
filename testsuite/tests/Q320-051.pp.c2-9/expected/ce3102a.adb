-- CE3102A.ADA

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
--     CHECK THAT STATUS_ERROR IS RAISED BY CREATE AND OPEN
--     IF THE GIVEN TEXT FILES ARE ALREADY OPEN.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH MODE OUT_FILE FOR TEXT FILES.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 07/25/83
--     JLH 08/07/87  COMPLETE REVISION OF TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3102a is

   Incomplete : exception;
   File : File_Type;

begin

   Test
     ("CE3102A",
      "CHECK THAT STATUS_ERROR IS RAISED " & "APPROPRIATELY FOR TEXT FILES");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
   end;

   begin
      Create (File, Out_File);
      Failed ("STATUS_ERROR NOT RAISED FOR CREATE - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR CREATE - 1");
   end;

   begin
      Create (File, In_File);
      Failed ("STATUS_ERROR NOT RAISED FOR CREATE - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR CREATE - 2");
   end;

   begin
      Create (File, Out_File, Legal_File_Name);
      Failed ("STATUS_ERROR NOT RAISED FOR CREATE - 3");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR CREATE - 3");
   end;

   begin
      Open (File, Out_File, Legal_File_Name);
      Failed ("STATUS_ERROR NOT RAISED FOR OPEN - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 1");
   end;

   begin
      Open (File, In_File, Legal_File_Name);
      Failed ("STATUS_ERROR NOT RAISED FOR OPEN - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 2");
   end;

   begin
      Open (File, In_File, Legal_File_Name (2, "CE3102A"));
      Failed ("STATUS_ERROR NOT RAISED FOR OPEN - 3");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 3");
   end;

   begin
      Create (File, In_File, Legal_File_Name (2, "CE3102A"));
      Failed ("STATUS_ERROR NOT RAISED FOR OPEN - 4");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 4");
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

end Ce3102a;

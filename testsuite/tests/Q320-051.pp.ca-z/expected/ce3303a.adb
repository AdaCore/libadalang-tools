-- CE3303A.ADA

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
--     PAGE_LENGTH RAISE STATUS_ERROR WHEN APPLIED TO A CLOSED FILE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     JLH 08/19/87  ADDED AN ATTEMPT TO CREATE AN EXTERNAL FILE;
--                   ADDED CHECKS TO THE SAME FOUR CASES WHICH EXIST
--                   IN TEST AGAINST ATTEMPTED CREATE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3303a is

   File : File_Type;
   Five : Count     := Count (Ident_Int (5));
   C    : Count;
   Item : Character := 'A';

begin

   Test
     ("CE3303A",
      "CHECK THAT SET_LINE_LENGTH, " & "SET_PAGE_LENGTH, LINE_LENGTH, AND " &
      "PAGE_LENGTH RAISE STATUS_ERROR " & "WHEN APPLIED TO A CLOSED FILE");

-- FILE NONEXISTANT

   begin
      Set_Line_Length (File, Five);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_LINE_LENGTH - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SET_LINE_LENGTH " & "- 1");
   end;

   begin
      Set_Page_Length (File, Five);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_PAGE_LENGTH - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SET_PAGE_LENGTH " & "- 1");
   end;

   begin
      C := Line_Length (File);
      Failed ("STATUS_ERROR NOT RAISED FOR LINE_LENGTH - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR LINE_LENGTH - 1");
   end;

   begin
      C := Page_Length (File);
      Failed ("STATUS_ERROR NOT RAISED FOR PAGE_LENGTH - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR PAGE_LENGTH - 1");
   end;

   begin
      Create (File, Out_File);
      Put (File, Item);
      Close (File);
   exception
      when Use_Error =>
         null;
   end;

   begin
      Set_Line_Length (File, Five);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_LINE_LENGTH - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SET_LINE_LENGTH " & "- 2");
   end;

   begin
      Set_Page_Length (File, Five);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_PAGE_LENGTH - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SET_PAGE_LENGTH " & "- 2");
   end;

   begin
      C := Line_Length (File);
      Failed ("STATUS_ERROR NOT RAISED FOR LINE_LENGTH - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR LINE_LENGTH - 2");
   end;

   begin
      C := Page_Length (File);
      Failed ("STATUS_ERROR NOT RAISED FOR PAGE_LENGTH - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR PAGE_LENGTH - 2");
   end;

   Result;

end Ce3303a;

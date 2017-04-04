-- CE3206A.ADA

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
--     CHECK THAT SET_INPUT AND SET_OUTPUT RAISE STATUS_ERROR WHEN
--     CALLED WITH A FILE PARAMETER DENOTING A CLOSED FILE.

-- HISTORY:
--     ABW 08/31/82
--     SPS 10/01/82
--     SPS 11/09/82
--     JLH 08/18/87  ADDED NEW CASES FOR SET_INPUT AND SET_OUTPUT.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3206a is

   File_In, File1 : File_Type;
   Item           : Character := 'A';

begin

   Test
     ("CE3206A",
      "CHECK THAT SET_INPUT AND SET_OUTPUT " &
      "RAISE STATUS_ERROR WHEN CALLED WITH A " &
      "FILE PARAMETER DENOTING A CLOSED FILE");

   begin
      Set_Input (File_In);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_INPUT - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SET_INPUT - 1");
   end;

   begin
      Set_Output (File_In);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_OUTPUT - 1");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SET_OUTPUT - 1");
   end;

   begin
      Create (File1, Out_File);
      Put (File1, Item);
      Close (File1);
   exception
      when Use_Error =>
         null;
   end;

   begin
      Set_Input (File1);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_INPUT - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SET_INPUT - 2");
   end;

   begin
      Set_Output (File1);
      Failed ("STATUS_ERROR NOT RAISED FOR SET_OUTPUT - 2");
   exception
      when Status_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SET_OUTPUT - 2");
   end;

   Result;

end Ce3206a;

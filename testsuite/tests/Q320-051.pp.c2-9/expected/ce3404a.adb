-- CE3404A.ADA

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
--     CHECK THAT END_OF_LINE RAISES MODE_ERROR WHEN APPLIED TO
--     AN OUT_FILE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 29/28/87  COMPLETELY REVISED.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3404a is

   My_File : File_Type;
   Bool    : Boolean;

begin

   Test
     ("CE3404A",
      "CHECK THAT END_OF_LINE RAISES MODE_ERROR " &
      "WHEN APPLIED TO AN OUT_FILE");

   begin
      Bool := End_Of_File (Current_Output);
      Failed ("MODE_ERROR NOT RAISED FOR CURRENT_OUTPUT - 1");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR " & "CURRENT_OUTPUT - 2");
   end;

   begin
      Bool := End_Of_File (Standard_Output);
      Failed ("MODE_ERROR NOT RAISED FOR STANDARD_OUTPUT - 3");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR " & "STANDARD_OUTPUT - 4");
   end;

   begin
      Create (My_File);
      begin
         Bool := End_Of_File (My_File);
         Failed ("MODE_ERROR NOT RAISED FOR MY_FILE - 5");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED FOR " & "MY_FILE - 6");

      end;

      Close (My_File);

   exception
      when Use_Error =>
         null;
   end;

   Result;

end Ce3404a;

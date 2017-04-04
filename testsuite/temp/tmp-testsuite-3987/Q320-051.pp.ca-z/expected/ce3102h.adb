-- CE3102H.ADA

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
--     CHECK THAT MODE_ERROR IS RAISED WHEN ATTEMPTING TO CHANGE
--     THE MODE OF A FILE SERVING AS THE CURRENT DEFAULT INPUT
--     OR DEFAULT OUTPUT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3102h is

   File1 : File_Type;
   Incomplete : exception;
   Item : Character := 'A';

begin

   Test
     ("CE3102H",
      "CHECK THAT MODE_ERROR IS RAISED WHEN " &
      "ATTEMPTING TO CHANGE THE MODE OF A FILE " &
      "SERVING AS THE CURRENT DEFAULT INPUT OR " &
      "DEFAULT OUTPUT FILE");

   begin
      Create (File1, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
   end;

   Set_Output (File1);

   begin
      Reset (File1, In_File);
      Failed ("MODE_ERROR NOT RAISED FOR RESET");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR RESET");
   end;

   Set_Output (Standard_Output);

   Put (File1, Item);
   Close (File1);

   begin
      Open (File1, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON OPEN");
         raise Incomplete;
   end;

   Set_Input (File1);

   begin
      Reset (File1, Out_File);
      Failed ("MODE_ERROR NOT RAISED FOR RESET");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR RESET");
   end;

   Set_Input (Standard_Input);

   begin
      Delete (File1);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3102h;

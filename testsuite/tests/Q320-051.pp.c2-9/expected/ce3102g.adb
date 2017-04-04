-- CE3102G.ADA

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
--     CANNOT BE DELETED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES, BUT DO NOT SUPPORT DELETION OF EXTERNAL FILES.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3102g is

   Incomplete : exception;
   File : File_Type;
   Var1 : Character := 'A';

begin

   Test
     ("CE3102G",
      "CHECK THAT USE_ERROR IS RAISED WHEN AN " &
      "EXTERNAL FILE CANNOT BE DELETED");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
      when others =>
         Not_Applicable ("UNEXPECTED EXCEPTION RAISED ON CREATE");
         raise Incomplete;
   end;

   Put (File, Var1);

   begin
      Delete (File);
      Not_Applicable ("DELETION OF EXTERNAL FILES ALLOWED");
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR DELETE");
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3102g;

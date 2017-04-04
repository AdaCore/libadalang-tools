-- CE3102I.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN CREATING A FILE OF MODE
--     OUT_FILE, WHEN OUT_FILE MODE IS NOT SUPPORTED FOR CREATE BY THE
--     IMPLEMENTATION FOR TEXT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH DO NOT
--     SUPPORT OUT_FILE FOR CREATE FOR TEXT_IO.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3102i is

   File1 : File_Type;

begin

   Test
     ("CE3102I",
      "CHECK THAT USE_ERROR IS RAISED WHEN MODE " &
      "OUT_FILE IS NOT SUPPORTED FOR THE OPERATION " &
      "OF CREATE FOR TEXT_IO");

   begin
      Create (File1, Out_File);
      Close (File1);
      Not_Applicable ("CREATE WITH MODE OUT_FILE ALLOWED");
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
   end;

   Result;

end Ce3102i;

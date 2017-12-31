-- CE2408A.ADA

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
--     CHECK THAT WRITE DOES NOT CAUSE AN EXCEPTION WHEN THE TO
--     PARAMETER IS GREATER THAN THE END POSITION.

--          1) FILE MODE IS OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF DIRECT FILES WITH MODE OUT_FILE.

-- HISTORY:
--     DLD 08/19/82
--     SPS 11/09/82
--     EG  05/16/85
--     GMT 08/05/87  ADDED A CHECK FOR USE_ERROR ON DELETE AND REMOVED
--                   THE OTHERS EXCEPTION AT THE BOTTOM OF THE FILE.

with Report; use Report;
with Direct_Io;

procedure Ce2408a is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;

   Dir_File : File_Type;
   Incomplete : exception;

begin

   Test
     ("CE2408A",
      "FOR FILES OF MODE OUT_FILE, CHECK THAT " &
      "WRITE DOES NOT CAUSE AN EXCEPTION  WHEN THE " &
      """TO"" PARAMETER IS GREATER THAN THE END " & "POSITION");

   -- CREATE TEST FILE

   begin
      Create (Dir_File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON CREATE WITH MODE " &
            "OUT_FILE FOR DIR_IO - 1");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON CREATE WITH " &
            "MODE OUT_FILE FOR DIR_IO - 2");
         raise Incomplete;
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED ON CREATE WITH " &
            "MODE OUT_FILE FOR DIR_IO - 3");
         raise Incomplete;
   end;

   -- FILL UP FILE

   Write (Dir_File, 3);
   Write (Dir_File, 4);
   Write (Dir_File, 5);
   Write (Dir_File, 6);

   -- WRITE WHERE TO IS LARGER THAN END OF FILE

   begin
      Write (Dir_File, 9, 7);
   exception
      when others =>
         Failed
           ("WRITE RAISED EXCEPTION WHEN TO " &
            "PARAMETER WAS BEYOND END - 4");
   end;

   begin
      Set_Index (Dir_File, 11);
      Write (Dir_File, 10);
   exception
      when others =>
         Failed
           ("SET_INDEX/WRITE RAISED EXCEPTION WHEN TO " &
            "PARAMETER EXCEEDS THE END POSITION - 5");
   end;

   -- DELETE TEST FILE

   begin
      Delete (Dir_File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2408a;

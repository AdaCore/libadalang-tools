-- CE2410A.ADA

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
--     CHECK THAT END_OF_FILE RAISES MODE_ERROR WHEN THE CURRENT
--     MODE IS OUT_FILE.

--          1) CHECK NON-TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     ABW 08/20/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     EG  11/02/84
--     EG  05/16/85
--     GMT 08/05/87  REVISED EXCEPTION HANDLING AND MOVED THE CASE FOR
--                   TEMPORARY FILES INTO CE2410B.ADA.

with Report; use Report;
with Direct_Io;

procedure Ce2410a is

   package Dir is new Direct_Io (Integer);
   use Dir;
   File1 : File_Type;
   Int   : Integer := Ident_Int (18);
   Bool  : Boolean;
   Incomplete : exception;

begin

   Test
     ("CE2410A",
      "CHECK THAT END_OF_FILE RAISES MODE_ERROR WHEN " &
      "THE CURRENT MODE IS OUT_FILE AND THE FILE IS " &
      "A NON-TEMPORARY FILE.");

   begin
      Create (File1, Out_File, Legal_File_Name);
   exception
      when Use_Error | Name_Error =>
         Not_Applicable
           ("CREATE WITH MODE OUT_FILE NOT " &
            "SUPPORTED FOR DIRECT FILES - 1");
         raise Incomplete;
   end;

   begin
      Bool := End_Of_File (File1);
      Failed ("MODE_ERROR NOT RAISED ON END_OF_FILE - 2");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED ON " & "END_OF_FILE - 3");
   end;

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

end Ce2410a;

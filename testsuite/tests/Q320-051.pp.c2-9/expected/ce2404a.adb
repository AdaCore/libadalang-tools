-- CE2404A.ADA

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
--     CHECK THAT READ RAISES MODE_ERROR WHEN THE CURRENT MODE IS
--     OUT_FILE.

--          A)  CHECK NON-TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF DIRECT FILES WITH MODE OUT_FILE.

-- HISTORY:
--     DLD 08/17/82
--     SPS 11/09/82
--     SPS 11/22/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 08/03/87  MOVED THE TEMP-FILE CASE TO CE2404B.ADA.

with Report; use Report;
with Direct_Io;

procedure Ce2404a is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;
   Dir_File_1 : File_Type;
   I          : Integer;
   Incomplete : exception;

begin
   Test
     ("CE2404A",
      "CHECK THAT READ RAISES MODE_ERROR WHEN THE " &
      "CURRENT MODE IS OUT_FILE AND THE FILE IS " &
      "A NON-TEMPORARY FILE");
   begin

      Create (Dir_File_1, Out_File, Legal_File_Name);

   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE - 1");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE - 2");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE - 3");
         raise Incomplete;
   end;

   begin
      Read (Dir_File_1, I);
      Failed ("MODE_ERROR NOT RAISED ON READ - 4");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED ON READ - 5");
   end;

   begin
      Delete (Dir_File_1);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2404a;

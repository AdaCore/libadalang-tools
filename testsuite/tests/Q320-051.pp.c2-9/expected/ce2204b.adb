-- CE2204B.ADA

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
--     CHECK THAT READ AND END_OF_FILE ARE FORBIDDEN FOR SEQUENTIAL
--     FILES OF MODE OUT_FILE.

--          A) CHECK NON-TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--      THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--      THE CREATION OF SEQUENTIAL FILES.

-- HISTORY:
--     DLD 08/17/82
--     SPS 08/24/82
--     SPS 110/9/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 07/24/87  SPLIT THIS TEST BY MOVING THE CODE FOR CHECKING
--                   TEMPORARY FILES INTO CE2204D.ADA.

with Report; use Report;
with Sequential_Io;

procedure Ce2204b is
begin
   Test
     ("CE2204B",
      "FOR A NON-TEMPORARY SEQUENTIAL FILE, CHECK " &
      "THAT MODE_ERROR IS RAISED BY READ AND " &
      "END_OF_FILE WHEN THE MODE IS OUT_FILE");
   declare
      package Seq_Io is new Sequential_Io (Integer);
      use Seq_Io;
      Seq_File : File_Type;
      X        : Integer;
      B        : Boolean;
      Incomplete : exception;
   begin
      begin
         Create (Seq_File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON CREATE - 1");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable ("NAME_ERROR RAISED ON CREATE - 2");
            raise Incomplete;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON CREATE - 3");
            raise Incomplete;
      end;

      Write (Seq_File, 5);

      begin                    -- THIS IS ONLY
         Reset (Seq_File);   -- AN ATTEMPT
      exception                -- TO RESET,
         when Use_Error =>   -- IF RESET
            null;          -- N/A THEN
      end;                     -- TEST IS
      -- NOT AFFECTED.
      begin
         Read (Seq_File, X);
         Failed ("MODE_ERROR NOT RAISED ON READ - 4");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON READ - 5");
      end;

      begin
         B := End_Of_File (Seq_File);
         Failed ("MODE_ERROR NOT RAISED ON END_OF_FILE - 6");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - END_OF_FILE - 7");
      end;

      begin
         Delete (Seq_File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2204b;

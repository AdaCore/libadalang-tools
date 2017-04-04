-- CE2204D.ADA

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

--          B) CHECK TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--      THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--      THE CREATION OF TEMPORARY SEQUENTIAL FILES.

-- HISTORY:
--     GMT 07/24/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2204d is
begin
   Test
     ("CE2204D",
      "FOR A TEMPORARY SEQUENTIAL FILE, CHECK THAT " &
      "MODE_ERROR IS RAISED BY READ AND END_OF_FILE " &
      "WHEN THE MODE IS OUT_FILE");
   declare
      package Seq_Io is new Sequential_Io (Integer);
      use Seq_Io;
      Ft : File_Type;
      X  : Integer;
      B  : Boolean;
      Incomplete : exception;
   begin
      begin
         Create (Ft);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON CREATE - 1");
            raise Incomplete;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON CREATE - 2");
            raise Incomplete;
      end;

      Write (Ft, 5);

      begin                    -- THIS IS ONLY
         Reset (Ft);         -- AN ATTEMPT
      exception                -- TO RESET,
         when Use_Error =>   -- IF RESET
            null;          -- N/A THEN
      end;                     -- TEST IS
      -- NOT AFFECTED.

      begin
         Read (Ft, X);
         Failed ("MODE_ERROR NOT RAISED ON READ - 3");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON READ - 4");
      end;

      begin
         B := End_Of_File (Ft);
         Failed ("MODE_ERROR NOT RAISED ON END_OF_FILE - 5");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - END_OF_FILE - 6");
      end;

      Close (Ft);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2204d;

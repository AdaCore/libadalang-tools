-- CE2206A.ADA

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
--     CHECK THAT READ FOR A SEQUENTIAL FILE RAISES END_ERROR WHEN
--     THERE ARE NO MORE ELEMENTS THAT CAN BE READ FROM THE GIVEN
--     FILE.  ALSO CHECK THAT END_OF_FILE CORRECTLY DETECTS THE END
--     OF A SEQUENTIAL FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     JLH 08/22/88  CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2206a is

   package Seq_Io is new Sequential_Io (Character);
   use Seq_Io;

   File : File_Type;
   Item : Character;
   Incomplete : exception;

begin

   Test
     ("CE2206A",
      "CHECK THAT READ FOR A SEQUENTIAL FILE RAISES " &
      "END_ERROR WHEN THERE ARE NO MORE ELEMENTS " &
      "THAT CAN BE READ FROM THE GIVEN FILE.  ALSO " &
      "CHECK THAT END_OF_FILE CORRECTLY DETECTS THE " &
      "END OF A SEQUENTIAL FILE");

   begin

      begin
         Create (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Write (File, 'A');
      Write (File, 'B');

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON OPEN WITH " & "MODE IN_FILE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON OPEN");
            raise Incomplete;
      end;

      Read (File, Item);
      if Item /= 'A' then
         Failed ("INCORRECT VALUE READ");
      end if;

      if End_Of_File (File) then
         Failed ("END_OF_FILE NOT DETECTED CORRECTLY - 1");
      end if;

      Read (File, Item);

      if not End_Of_File (File) then
         Failed ("END_OF_FILE NOT DETECTED CORRECTLY - 2");
      end if;

      begin
         Read (File, Item);
         Failed ("END_ERROR NOT RAISED FOR READ");
      exception
         when End_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON READ");
      end;

      begin
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce2206a;

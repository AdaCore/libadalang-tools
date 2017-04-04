-- CE2106A.ADA

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
--     CHECK THAT AFTER A SUCCESSFUL DELETE OF AN EXTERNAL FILE, THE
--     NAME OF THE FILE CAN BE USED IN A CREATE OPERATION.

--          A) SEQUENTIAL FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH OUT_FILE MODE FOR SEQUENTIAL FILES AND
--     DELETION OF EXTERNAL FILES.

-- HISTORY:
--     SPS 08/25/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 02/12/86  SPLIT TEST.  PUT DIRECT_IO INTO CE2106B.ADA.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     SPW 08/07/87  INSERTED ALLOWABLE EXCEPTION USE_ERROR ON
--                   DELETE.

with Report; use Report;
with Sequential_Io;

procedure Ce2106a is

begin

   Test
     ("CE2106A",
      "CHECK THAT AN EXTERNAL FILE CAN BE CREATED " &
      "AFTER AN EXTERNAL FILE WITH SAME NAME HAS " &
      "BEEN DELETED FOR SEQUENTIAL_IO");

   declare
      package Seq is new Sequential_Io (Integer);
      use Seq;
      Fl1      : File_Type;
      Fl2      : File_Type;
      T_Failed : Boolean := False;
      D_File   : Boolean := False;
   begin
      begin
         Create (Fl1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; SEQUENTIAL " & "CREATE WITH OUT_FILE MODE");
            T_Failed := True;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; SEQUENTIAL " & "CREATE WITH OUT_FILE MODE");
            T_Failed := True;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED; SEQUENTIAL " & "CREATE");
            T_Failed := True;
      end;

      if not T_Failed then
         begin
            Delete (Fl1);
         exception
            when Use_Error =>
               Not_Applicable
                 ("DELETION OF EXTERNAL FILE " & "IS NOT SUPPORTED");
               T_Failed := True;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED ON " & "DELETE");
               T_Failed := True;
         end;
      end if;

      if not T_Failed then
         begin
            Create (Fl2, Out_File, Legal_File_Name);
            D_File := True;
         exception
            when Use_Error =>
               Failed ("USE_ERROR FOR RECREATE - SEQ");
            when others =>
               Failed ("UNABLE TO RECREATE FILE AFTER " & "DELETION - SEQ");
         end;

         if D_File then
            begin
               Delete (Fl2);
            exception
               when Use_Error =>
                  Failed ("USE_ERROR FOR DELETE - SEQ");
            end;
         end if;
      end if;
   end;

   Result;

end Ce2106a;

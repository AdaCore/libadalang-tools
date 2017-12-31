-- CE2204A.ADA

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
--     CHECK THAT WRITE IS FORBIDDEN FOR SEQUENTIAL FILES OF
--     MODE IN_FILE.

--          A)  CHECK NON-TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     DLD 08/17/82
--     SPS 08/24/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     JBG 03/30/84
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 07/27/87  SPLIT THIS TEST BY MOVING THE CODE FOR CHECKING
--                   TEMPORARY FILES INTO CE2204C.ADA.

with Report; use Report;
with Sequential_Io;

procedure Ce2204a is
   Incomplete : exception;
begin
   Test
     ("CE2204A",
      "CHECK THAT MODE_ERROR IS RAISED BY WRITE " &
      "WHEN THE MODE IS IN_FILE AND THE FILE " & "IS A NON-TEMPORARY FILE");
   declare
      package Seq_Io is new Sequential_Io (Integer);
      use Seq_Io;
      Seq_File : File_Type;
      Var1     : Integer := 5;
   begin
      begin
         Create (Seq_File, Out_File, Legal_File_Name (1, "CE2204A"));
         Write (Seq_File, Var1);
         Close (Seq_File);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED; " & "SEQUENTIAL CREATE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable ("NAME_ERROR RAISED; " & "SEQUENTIAL CREATE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED; " & "SEQUENTIAL CREATE");
            raise Incomplete;
      end;

      begin
         Open (Seq_File, In_File, Legal_File_Name (1, "CE2204A"));
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR  RAISED ON THE " & "OPENING OF A SEQUENTIAL " &
               "NON-TEMPORARY FILE");
            raise Incomplete;
      end;

      begin
         Write (Seq_File, 3);
         Failed ("MODE_ERROR NOT RAISED - NAMED FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - NAMED FILE");
      end;

      begin
         Delete (Seq_File);
      exception
         when Use_Error =>
            null;
      end;

   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2204a;

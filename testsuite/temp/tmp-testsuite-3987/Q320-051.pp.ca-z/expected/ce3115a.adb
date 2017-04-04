-- CE3115A.ADA

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
--     CHECK THAT RESETTING ONE OF A MULTIPLE OF INTERNAL FILES
--     ASSOCIATED WITH THE SAME EXTERNAL FILE HAS NO EFFECT ON ANY
--     OF THE OTHER INTERNAL FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST APPLIES ONLY TO IMPLEMENTATIONS WHICH SUPPORT MULTIPLE
--     INTERNAL FILES ASSOCIATED WITH THE SAME EXTERNAL FILE AND
--     RESETTING OF THESE MULTIPLE INTERNAL FILES FOR TEXT FILES.

-- HISTORY:
--     DLD 08/16/82
--     SPS 11/09/82
--     JBG 06/04/84
--     EG  11/19/85  MADE TEST INAPPLICABLE IF CREATE USE_ERROR.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE RESULT WHEN
--                   FILES NOT SUPPORTED.
--     GMT 08/25/87  COMPLETELY REVISED.
--     EDS 12/01/97  ADD NAME_ERROR HANDLER TO OUTPUT NOT_APPLICABLE RESULT.
--     RLB 09/29/98  MADE MODIFICATION TO AVOID BUFFERING PROBLEMS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3115a is

begin

   Test
     ("CE3115A",
      "CHECK THAT RESETTING ONE OF A MULTIPLE OF " &
      "INTERNAL FILES ASSOCIATED WITH THE SAME " &
      "EXTERNAL FILE HAS NO EFFECT ON ANY OF THE " &
      "OTHER INTERNAL FILES");

   declare
      Txt_File_One : Text_Io.File_Type;
      Txt_File_Two : Text_Io.File_Type;

      Ch : Character := 'A';

      Incomplete : exception;

      procedure Txt_Cleanup is
         File1_Open : Boolean := Is_Open (Txt_File_One);
         File2_Open : Boolean := Is_Open (Txt_File_Two);
      begin
         if File1_Open and File2_Open then
            Close (Txt_File_Two);
            Delete (Txt_File_One);
         elsif File1_Open then
            Delete (Txt_File_One);
         elsif File2_Open then
            Delete (Txt_File_Two);
         end if;
      exception
         when Text_Io.Use_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED " & "IN CLEANUP - 1");
      end Txt_Cleanup;

   begin

      begin -- CREATE FIRST FILE

         Create (Txt_File_One, Out_File, Legal_File_Name);
         Put (Txt_File_One, Ch);

      exception
         when Text_Io.Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; CREATE OF " &
               "EXTERNAL FILENAME IS NOT " &
               "SUPPORTED - 2");
            raise Incomplete;
         when Text_Io.Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; CREATE OF " &
               "EXTERNAL FILENAME IS NOT " &
               "SUPPORTED - 3");
            raise Incomplete;

      end; -- CREATE FIRST FILE

      begin -- OPEN SECOND FILE

         Open (Txt_File_Two, In_File, Legal_File_Name);

      exception

         when Text_Io.Use_Error =>
            Not_Applicable
              ("MULTIPLE INTERNAL FILES ARE NOT " &
               "SUPPORTED WHEN ONE IS MODE " &
               "OUT_FILE AND THE OTHER IS MODE " &
               "IN_FILE - 4" &
               " - USE_ERROR RAISED ");
            Txt_Cleanup;
            raise Incomplete;

         when Text_Io.Name_Error =>
            Not_Applicable
              ("MULTIPLE INTERNAL FILES ARE NOT " &
               "SUPPORTED WHEN ONE IS MODE " &
               "OUT_FILE AND THE OTHER IS MODE " &
               "IN_FILE - 4" &
               " - NAME_ERROR RAISED ");
            Txt_Cleanup;
            raise Incomplete;

      end; -- OPEN SECOND FILE
      Flush (Txt_File_One); -- AVOID BUFFERING PROBLEMS.

      Ch := 'B';
      Get (Txt_File_Two, Ch);
      if Ch /= 'A' then
         Failed ("INCORRECT VALUE FOR GET - 5");
      end if;

      begin -- INITIALIZE FIRST FILE TO CHECK POINTER RESETTING

         Reset (Txt_File_One);
         if Mode (Txt_File_One) /= Out_File then
            Failed ("FILE WAS NOT RESET - 6");
         end if;
         if Mode (Txt_File_Two) /= In_File then
            Failed
              ("RESETTING OF ONE INTERNAL FILE " &
               "AFFECTED THE OTHER INTERNAL FILE - 7");
         end if;

      exception

         when Text_Io.Use_Error =>
            Not_Applicable
              ("RESETTING OF EXTERNAL FILE FOR " &
               "OUT_FILE MODE IS " &
               " NOT SUPPORTED - 8");
            Txt_Cleanup;
            raise Incomplete;

      end; -- INITIALIZE FIRST FILE TO CHECK POINTER RESETTING

      -- PERFORM SOME I/O ON THE FIRST FILE

      Put (Txt_File_One, 'C');
      Put (Txt_File_One, 'D');
      Put (Txt_File_One, 'E');
      Close (Txt_File_One);

      begin
         Open (Txt_File_One, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("MULTIPLE INTERNAL FILES NOT " &
               "SUPPORTED WHEN BOTH FILES HAVE " &
               "IN_FILE MODE - 9");
            raise Incomplete;
      end;

      Get (Txt_File_One, Ch);
      Get (Txt_File_One, Ch);

      begin -- INITIALIZE SECOND FILE AND PERFORM SOME I/O

         Close (Txt_File_Two);
         Open (Txt_File_Two, In_File, Legal_File_Name);

      exception

         when Text_Io.Use_Error =>
            Failed
              ("MULTIPLE INTERNAL FILES SHOULD STILL " & "BE ALLOWED - 10");
            Txt_Cleanup;
            raise Incomplete;

      end; -- INITIALIZE SECOND FILE AND PERFORM SOME I/O

      begin -- RESET FIRST FILE AND CHECK EFFECTS ON SECOND FILE

         Get (Txt_File_Two, Ch);
         if Ch /= 'C' then
            Failed ("INCORRECT VALUE FOR GET OPERATION - 11");
         end if;

         Reset (Txt_File_One);
         Get (Txt_File_Two, Ch);
         if Ch /= 'D' then
            Failed
              ("RESETTING INDEX OF ONE TEXT FILE " &
               "RESETS THE OTHER ASSOCIATED FILE - 12");
         end if;

      exception

         when Text_Io.Use_Error =>
            Failed ("RESETTING SHOULD STILL BE SUPPORTED - 13");
            Txt_Cleanup;
            raise Incomplete;

      end; -- RESET FIRST FILE AND CHECK EFFECTS ON SECOND FILE

      Txt_Cleanup;

   exception

      when Incomplete =>
         null;

   end;

   Result;

end Ce3115a;

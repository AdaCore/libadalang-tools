-- CE3605D.ADA

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
--     CHECK THAT PUT DOES NOT RAISE LAYOUT_ERROR WHEN THE NUMBER OF
--     CHARACTERS TO BE OUTPUT EXCEEDS THE LINE LENGTH.
--     CHECK THAT PUT HAS THE EFFECT OF NEW_LINE (AS WELL AS
--     OUTPUTTING THE ITEM) WHEN THE NUMBER OF CHARACTERS TO BE OUTPUT
--     OVERFLOWS A BOUNDED LINE LENGTH.
--     CHECK THAT PUT WITH A NULL STRING PERFORMS NO OPERATION.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 12/28/82
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;
procedure Ce3605d is
   Incomplete : exception;

begin

   Test
     ("CE3605D",
      "CHECK THAT LAYOUT_ERROR IS NOT RAISED BY PUT " & "FOR STRING");

   declare
      Ft : File_Type;
      Lc : Positive_Count;
   begin

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE");
            raise Incomplete;
      end;

      Set_Line_Length (Ft, 5);

      begin
         Put (Ft, "STRING");

         if Line (Ft) /= 2 then
            Failed
              ("LINE COUNT WAS" & Count'Image (Line (Ft)) & " INSTEAD OF 2");
         end if;

         if Col (Ft) /= 2 then
            Failed
              ("COLUMN COUNT WAS" & Count'Image (Col (Ft)) & " INSTEAD OF 2");
         end if;

      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED - 1");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 1");

      end;

      Put (Ft, "NEW");

      if Line (Ft) /= 2 then
         Failed
           ("LINE COUNT WRONG - 2; WAS" & Count'Image (Line (Ft)) &
            " INSTEAD OF 2");
      end if;

      if Col (Ft) /= 5 then
         Failed
           ("COL COUNT WRONG - 2; WAS" & Count'Image (Col (Ft)) &
            " INSTEAD OF 5");
      end if;

      begin
         Put (Ft, "STR");
         if Line (Ft) /= 3 then
            Failed
              ("PUT STRING WHEN IN MIDDLE OF " &
               "LINE DOES NOT HAVE EFFECT OF " & "NEW_LINE; LINE COUNT IS" &
               Count'Image (Line (Ft)));
         end if;

         if Col (Ft) /= 3 then
            Failed
              ("COL COUNT WRONG - 3; WAS" & Count'Image (Col (Ft)) &
               " INSTEAD OF 3");
         end if;

      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED - 2");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 2");
      end;

      Put (Ft, "ING");

      if Line (Ft) /= 3 then
         Failed
           ("LINE COUNT WRONG - 3; WAS" & Count'Image (Line (Ft)) &
            " INSTEAD OF 3");
      end if;

      if Col (Ft) /= 6 then
         Failed
           ("COL COUNT WRONG - 3;  WAS" & Count'Image (Col (Ft)) &
            " INSTEAD OF 6");
      end if;

      begin
         Put (Ft, "");

         if Line (Ft) /= 3 then
            Failed
              ("LINE COUNT WRONG - 3; WAS" & Count'Image (Line (Ft)) &
               " INSTEAD OF 3");
         end if;

         if Col (Ft) /= 6 then
            Failed
              ("COL COUNT WRONG - 3;  WAS" & Count'Image (Col (Ft)) &
               " INSTEAD OF 6");
         end if;

      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED - 3");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 3");
      end;

      Check_File (Ft, "STRIN#" & "GNEWS#" & "TRING#@%");

      begin
         Delete (Ft);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3605d;

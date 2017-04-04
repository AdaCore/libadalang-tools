-- CE3905B.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES RAISE MODE_ERROR WHEN THE
--     MODE OF THE FILE SPECIFIED IS OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT CREATE FOR TEMP FILES WITH OUT_FILE.

-- HISTORY:
--     SPS 10/07/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3905b is
   Incomplete : exception;

begin

   Test
     ("CE3905B",
      "CHECK THAT ENUMERATION_IO GET RAISES " &
      "MODE_ERROR WHEN THE MODE OF THE FILE IS " &
      "OUT_FILE");

   declare
      Ft : File_Type;
      type Color is (Red, Blue, Green, Yellow);
      X : Color;
      package Color_Io is new Enumeration_Io (Color);
      use Color_Io;
   begin

      begin
         Create (Ft, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " &
               "FOR TEMP FILES WITH OUT_FILE " &
               "MODE");
            raise Incomplete;
      end;

      begin
         Get (Ft, X);
         Failed ("MODE_ERROR NOT RAISED - FILE");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FILE");
      end;

      begin
         Get (Standard_Output, X);
         Failed ("MODE_ERROR NOT RAISED - STANDARD_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - STANDARD_OUTPUT");
      end;

      begin
         Get (Current_Output, X);
         Failed ("MODE_ERROR NOT RAISED - CURRENT_OUTPUT");
      exception
         when Mode_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - CURRENT_OUTPUT");
      end;

      Close (Ft);

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3905b;

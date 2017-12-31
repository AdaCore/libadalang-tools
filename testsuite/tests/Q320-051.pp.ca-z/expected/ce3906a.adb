-- CE3906A.ADA

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
--     CHECK THAT PUT FOR ENUMERATION TYPES CAN OPERATE ON FILES OF
--     MODE OUT_FILE AND THAT WHEN NO FILE PARAMETER IS SPECIFIED
--     THE CURRENT DEFAULT OUTPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEMPORARY TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     SPS 01/03/83
--     SPS 02/18/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/17/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3906a is
   Incomplete : exception;

begin

   Test
     ("CE3906A",
      "CHECK THAT PUT FOR ENUMERATION TYPES CAN " &
      "OPERATE ON FILES OF MODE OUT_FILE AND THAT " &
      "WHEN NO FILE PARAMETER IS SPECIFIED THE " &
      "CURRENT DEFAULT OUTPUT FILE IS USED. CHECK " &
      "THAT ENUMERATION_IO PUT OPERATES ON OUT_FILE " & "FILES");

   declare
      Ft1, Ft2 : File_Type;
      type Color is (Rose, Vanilla, Charcoal, Chocolate);
      Crayon : Color := Rose;
      package Color_Io is new Enumeration_Io (Color);
      use Color_Io;
   begin

      begin
         Create (Ft1, Out_File);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " &
               "FOR TEMP FILES WITH OUT_FILE " & "MODE - 1");
            raise Incomplete;
      end;

      Create (Ft2, Out_File);

      Set_Output (Ft2);

      Put (Ft1, Crayon);
      New_Line (Ft1);
      Put (Ft1, Chocolate);

      Crayon := Charcoal;

      Put (Crayon);
      New_Line;
      Put (Vanilla);

-- CHECK OUTPUT

      Set_Output (Standard_Output);
      Comment ("CHECKING FT1");
      Check_File (Ft1, "ROSE#CHOCOLATE#@%");

      Comment ("CHECKING FT2");
      Check_File (Ft2, "CHARCOAL#VANILLA#@%");

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3906a;

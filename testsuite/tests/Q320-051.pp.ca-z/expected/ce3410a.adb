-- CE3410A.ADA

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
--     CHECK THAT SET_LINE RAISES LAYOUT_ERROR IF THE PAGE LENGTH IS
--     BOUNDED AND THE GIVEN LINE POSITION EXCEEDS THE PAGE LENGTH
--     FOR FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3410a is

   Incomplete : exception;
   File  : File_Type;
   Three : Positive_Count := Positive_Count (Ident_Int (3));
   Four  : Positive_Count := Positive_Count (Ident_Int (4));

begin

   Test
     ("CE3410A", "CHECK THAT SET_LINE RAISES " & "LAYOUT_ERROR APPROPRIATELY");

   begin
      Create (File);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE FOR " &
            "TEMPORARY FILE WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   Set_Page_Length (File, Three);

   begin
      Set_Line (File, Four);
      Failed ("LAYOUT ERROR NOT RAISED FOR SET_LINE");
   exception
      when Layout_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR SET_LINE");
   end;

   Close (File);

   Result;

exception
   when Incomplete =>
      Result;

end Ce3410a;

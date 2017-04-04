-- CE3906F.ADA

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
--     CHECK THAT THE SET PARAMETER AFFECTS THE CASE OF IDENTIFIERS,
--     BUT NOT CHARACTER LITERALS.  CHECK THAT CHARACTER LITERALS ARE
--     ENCLOSED IN APOSTROPHES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     JBG 12/30/82
--     VKG 01/12/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/18/87  CORRECTED EXCEPTION HANDLING.

with Text_Io; use Text_Io;
with Report;  use Report;
with Check_File;

procedure Ce3906f is

   type Enum is (Redish, Greenish, Yellowish);
   package Enum_Io is new Enumeration_Io (Enum);
   package Char_Io is new Enumeration_Io (Character);
   use Enum_Io;
   use Char_Io;
   Incomplete : exception;
   Ft : File_Type;

begin

   Test ("CE3906F", "CHECK THE CASE OF ENUMERATION IO OUTPUT");

   begin
      Create (Ft);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED; TEXT " &
            "CREATE FOR TEMP FILE WITH " &
            "OUT_FILE MODE - 1");
         raise Incomplete;
   end;

   if Enum_Io.Default_Width /= 0 then
      Failed ("INITIAL DEFAULT WIDTH INCORRECT");
   end if;

   if Char_Io.Default_Setting /= Upper_Case then
      Failed ("INITIAL DEFAULT_SETTING INCORRECT");
   end if;

   Put (Ft, 'A', Set => Lower_Case);
   New_Line (Ft);
   Put (Ft, 'a', Set => Lower_Case);
   New_Line (Ft);
   Put (Ft, Redish, Set => Lower_Case);
   New_Line (Ft);
   Enum_Io.Default_Setting := Lower_Case;
   Char_Io.Put (Ft, 'C');
   New_Line (Ft);
   Char_Io.Put (Ft, 'b');
   New_Line (Ft);
   Put (Ft, Redish);
   New_Line (Ft);
   Put (Ft, Greenish, Set => Lower_Case);
   New_Line (Ft);
   Put (Ft, Yellowish, Set => Upper_Case);

   Check_File (Ft, "'A'#'a'#redish#'C'#'b'#redish#greenish#" & "YELLOWISH#@%");

   Result;

exception
   when Incomplete =>
      Result;

end Ce3906f;

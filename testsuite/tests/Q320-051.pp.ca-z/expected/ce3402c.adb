-- CE3402C.ADA

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
--     CHECK THAT NEW_LINE INCREMENTS THE CURRENT PAGE BY ONE AND
--     SETS THE CURRENT LINE NUMBER TO ONE WHEN THE PAGE LENGTH IS
--     BOUNDED AND THE LINE NUMBER WOULD HAVE EXCEEDED THE
--     MAXIMUM PAGE LENGTH.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 09/01/82
--     SPS 11/30/82
--     SPS 01/24/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/19/87  ADDED ORIGINAL_LINE_LENGTH AND
--                   ORIGINAL_PAGE_LENGTH VARIABLES AND CLOSED FILE.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3402c is

   Incomplete : exception;
   File                 : File_Type;
   One                  : Positive_Count := Positive_Count (Ident_Int (1));
   Two                  : Positive_Count := Positive_Count (Ident_Int (2));
   Three                : Positive_Count := Positive_Count (Ident_Int (3));
   Char                 : Character      := ('C');
   Item_Char            : Character;
   Original_Line_Length : Count          := Line_Length;
   Original_Page_Length : Count          := Page_Length;

begin

   Test ("CE3402C", "CHECK END_OF_PAGE BEHAVIOR OF NEW_LINE");

   begin
      Create (File);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED; TEXT CREATE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
         raise Incomplete;
   end;

   Set_Line_Length (File, Three);
   Set_Page_Length (File, Two);

   for I in 1 .. 6 loop
      Put (File, Char);
   end loop;

   New_Line (File);

   if Page (File) /= Two then
      Failed ("PAGE NOT INCREMENTED BY ONE");
   end if;

   if Line (File) /= One then
      Failed ("LINE NOT SET TO ONE");
   end if;

   New_Line (File, 7);
   if Page (File) /= Positive_Count (Ident_Int (5)) then
      Failed ("MULTIPLE PAGES NOT CREATED BY NEW_LINE");
   end if;

   Set_Line_Length (File, Original_Line_Length);
   Set_Page_Length (File, Original_Page_Length);
   Check_File (File, "CCC#CCC#@##@##@##@#@%");

   Close (File);

   Result;

exception
   when Incomplete =>
      Result;

end Ce3402c;

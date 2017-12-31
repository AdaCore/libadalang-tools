-- CHECK_FILE.ADA
--
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
--
-- THIS IS A PROCEDURE USED BY MANY OF THE CHAPTER 14 TESTS TO CHECK THE
-- CONTENTS OF A TEXT FILE.

-- THIS PROCEDURE ASSUMES THE FILE PARAMETER PASSED TO IT IS AN OPEN TEXT FILE.

-- THE STRING PARAMETER CONTAINS THE CHARACTERS THAT ARE SUPPOSED TO BE IN
-- THE TEXT FILE. A '#' CHARACTER IS USED IN THE STRING TO DENOTE THE END OF A
-- LINE. A '@' CHARACTER IS USED TO DENOTE THE END OF A PAGE. A '%' CHARACTER
-- IS USED TO DENOTE THE END OF THE TEXT FILE. THESE SYMBOLS SHOULD NOT BE USED
-- AS TEXT OUTPUT.

-- SPS 11/30/82
-- JBG 2/3/83

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Check_File (File : in out File_Type; Contents : String) is

   X                           : Character;
   Col_Count                   : Positive_Count := 1;
   Line_Count                  : Positive_Count := 1;
   Page_Count                  : Positive_Count := 1;
   Trailing_Blanks_Msg_Written : Boolean        := False;
   Stop_Processing : exception;

   procedure Check_End_Of_Line (Expect_End_Of_Page : Boolean) is
   begin

-- SKIP OVER ANY TRAILING BLANKS. AN IMPLEMENTATION CAN LEGALLY APPEND BLANKS
-- TO THE END OF ANY LINE.

      while not End_Of_Line (File) loop
         Get (File, X);
         if X /= ' ' then
            Failed
              ("FROM CHECK_FILE: END OF LINE EXPECTED - " & X &
               " ENCOUNTERED");
            raise Stop_Processing;
         else
            if not Trailing_Blanks_Msg_Written then
               Comment
                 ("FROM CHECK_FILE: " & "THIS IMPLEMENTATION PADS " &
                  "LINES WITH BLANKS");
               Trailing_Blanks_Msg_Written := True;
            end if;
         end if;
      end loop;

      if Line_Count /= Line (File) then
         Failed
           ("FROM CHECK_FILE: " & "LINE COUNT INCORRECT - EXPECTED " &
            Positive_Count'Image (Line_Count) & " GOT FROM FILE " &
            Positive_Count'Image (Line (File)));
      end if;

-- NOTE: DO NOT SKIP_LINE WHEN AT END OF PAGE BECAUSE SKIP_LINE WILL
--        ALSO SKIP THE PAGE TERMINATOR.  SEE RM 14.3.5 PARAGRAPH 1.

      if not Expect_End_Of_Page then
         if End_Of_Page (File) then
            Failed ("FROM CHECK_FILE: PREMATURE END OF PAGE");
            raise Stop_Processing;
         else
            Skip_Line (File);
            Line_Count := Line_Count + 1;
         end if;
      end if;
      Col_Count := 1;
   end Check_End_Of_Line;

   procedure Check_End_Of_Page is
   begin
      if not End_Of_Page (File) then
         Failed ("FROM CHECK_FILE: " & "END_OF_PAGE NOT WHERE EXPECTED");
         raise Stop_Processing;
      else
         if Page_Count /= Page (File) then
            Failed
              ("FROM CHECK_FILE: " & "PAGE COUNT INCORRECT - EXPECTED " &
               Positive_Count'Image (Page_Count) & " GOT FROM FILE " &
               Positive_Count'Image (Page (File)));
         end if;

         Skip_Page (File);
         Page_Count := Page_Count + 1;
         Line_Count := 1;
      end if;
   end Check_End_Of_Page;

begin

   Reset (File, In_File);
   Set_Line_Length (Standard_Output, 0);
   Set_Page_Length (Standard_Output, 0);

   for I in 1 .. Contents'Length loop

      begin
         case Contents (I) is
            when '#' =>
               Check_End_Of_Line (Contents (I + 1) = '@');
            when '@' =>
               Check_End_Of_Page;
            when '%' =>
               if not End_Of_File (File) then
                  Failed
                    ("FROM CHECK_FILE: " & "END_OF_FILE NOT WHERE EXPECTED");
                  raise Stop_Processing;
               end if;
            when others =>
               if Col_Count /= Col (File) then
                  Failed
                    ("FROM CHECK_FILE: " & "COL COUNT INCORRECT - " &
                     "EXPECTED " & Positive_Count'Image (Col_Count) &
                     " GOT FROM " & "FILE " &
                     Positive_Count'Image (Col (File)));
               end if;
               Get (File, X);
               Col_Count := Col_Count + 1;
               if X /= Contents (I) then
                  Failed
                    ("FROM CHECK_FILE: " & "FILE DOES NOT CONTAIN CORRECT " &
                     "OUTPUT - EXPECTED " & Contents (I) & " - GOT " & X);
                  raise Stop_Processing;
               end if;
         end case;
      exception
         when Stop_Processing =>
            Comment
              ("FROM CHECK_FILE: " & "LAST CHARACTER IN FOLLOWING STRING " &
               "REVEALED ERROR: " & Contents (1 .. I));
            exit;
      end;

   end loop;

exception
   when Status_Error =>
      Failed
        ("FROM CHECK_FILE: " &
         "STATUS_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when Mode_Error =>
      Failed
        ("FROM CHECK_FILE: " & "MODE_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when Name_Error =>
      Failed
        ("FROM CHECK_FILE: " & "NAME_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when Use_Error =>
      Failed
        ("FROM CHECK_FILE: " & "USE_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when Device_Error =>
      Failed
        ("FROM CHECK_FILE: " &
         "DEVICE_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when End_Error =>
      Failed
        ("FROM CHECK_FILE: " & "END_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when Data_Error =>
      Failed
        ("FROM CHECK_FILE: " & "DATA_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when Layout_Error =>
      Failed
        ("FROM CHECK_FILE: " &
         "LAYOUT_ERROR RAISED - FILE CHECKING INCOMPLETE");
   when others =>
      Failed
        ("FROM CHECK_FILE: " &
         "SOME EXCEPTION RAISED - FILE CHECKING INCOMPLETE");

end Check_File;

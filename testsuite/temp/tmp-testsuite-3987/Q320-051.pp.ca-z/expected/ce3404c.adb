-- CE3404C.ADA

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
--     CHECK THAT END_OF_LINE RETURNS THE CORRECT VALUE WHEN POSITIONED
--     AT THE BEGINNING AND THE END OF A LINE, AND WHEN POSITIONED JUST
--     BEFORE THE FILE TERMINATOR.

--          CASE 1)  BOUNDED LINE LENGTH

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 09/22/87  REMOVED DEPENDENCE ON RESET AND MOVED THE CHECK
--                   FOR UNBOUNDED LINE_LENGTH TO CE3404D.ADA.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3404c is
   Incomplete : exception;
   My_File       : File_Type;
   Item_Char     : Character;
   Char          : Character      := ('C');
   Ten           : Positive_Count := Positive_Count (Ident_Int (10));
   Blank_Counter : Natural        := 0;

begin

   Test
     ("CE3404C",
      "CHECK THAT END_OF_LINE RETURNS THE CORRECT " &
      "VALUE WHEN POSITIONED AT THE BEGINNING " &
      "AND THE END OF A LINE, AND WHEN POSITIONED " &
      "JUST BEFORE THE FILE TERMINATOR");

--   CREATE AND INITIALIZE TEST FILE WITH BOUNDED LINE LENGTH

   begin
      Create (My_File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   Set_Line_Length (My_File, Ten);

   for I in 1 .. 5 loop
      Put (My_File, Char);
   end loop;
   New_Line (My_File);
   Put (My_File, 'B');

   Close (My_File);

   begin
      Open (My_File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

--   BEGIN THE TEST

   if End_Of_Line (My_File) then
      Failed ("END_OF_LINE: INCORRECT VALUE AT FIRST POSITION - 5");
   end if;

   if Col (My_File) /= 1 then
      Failed ("EOL MODIFIED COL NUMBER - 6");
   end if;

   for I in 1 .. 4 loop
      Get (My_File, Item_Char);
   end loop;

   if End_Of_Line (My_File) then
      Failed ("END_OF_LINE: INCORRECT VALUE AT FIFTH POSITION - 7");
   end if;

   Get (My_File, Item_Char);

   while not End_Of_Line (My_File) loop
      Get (My_File, Item_Char);
      if Item_Char = ' ' then
         Blank_Counter := Blank_Counter + 1;
      else
         Failed
           ("STRING WAS PADDED WITH SOMETHING OTHER THAN " & "BLANKS - 8");
      end if;
   end loop;

   if Blank_Counter > 5 then
      Failed ("TOO MANY BLANKS WERE USED FOR PADDING - 9");
   end if;

   if Line (My_File) /= 1 then
      Failed ("EOL SKIPPED LINE TERMINATOR - 10");
   end if;

   if not End_Of_Line (My_File) then
      Failed ("EOL SKIPPED LINE TERMINATOR - 11");
   end if;

   Skip_Page (My_File);

   if Page (My_File) /= 2 then
      Failed ("INCORRECT PAGE NUMBER");
   end if;

   if not End_Of_Line (My_File) then
      Failed
        ("INCORRECT VALUE WHEN POSITIONED JUST BEFORE FILE " & "TERMINATOR");
   end if;

   begin
      Delete (My_File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3404c;

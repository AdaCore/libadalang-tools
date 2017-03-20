-- CE3404B.ADA

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
--     CHECK THAT END_OF_LINE OPERATES ON THE CURRENT DEFAULT INPUT FILE
--     IF NO FILE IS SPECIFIED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW  08/26/82
--     SPS  09/17/82
--     SPS  11/11/82
--     TBN  11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                    RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT  09/22/87  CREATED A NON-TEMP FILE, REMOVED DEPENDENCE ON
--                    RESET, AND CHECKED THE VALUE OF THE CHAR READ.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3404b is

   Incomplete : exception;
   My_File    : File_Type;
   Loop_Count : Integer   := 0;
   Bool       : Boolean;
   Char       : Character := ('C');

begin

   Test
     ("CE3404B",
      "CHECK THAT END_OF_LINE OPERATES ON THE " &
      "CURRENT DEFAULT INPUT FILE IF NO FILE " &
      "IS SPECIFIED");

--   CREATE AND INITIALIZE THE FILE

   begin
      Create (My_File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE - 1");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE - 2");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE - 3");
         raise Incomplete;
   end;

   for I in 1 .. 3 loop
      Put (My_File, Char);
   end loop;
   New_Line (My_File);
   Put (My_File, Char);

   Close (My_File);

   begin
      Open (My_File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE ERROR RAISED ON ATTEMPT TO " &
            "RE-OPEN WITH MODE OF IN_FILE - 4");
         raise Incomplete;
   end;

   Set_Input (My_File);

--   START THE TEST

   loop
      Get (Char);
      if Char /= 'C' then
         Failed ("CHAR READ FROM FILE HAS WRONG VALUE - 5");
         raise Incomplete;
      end if;
      exit when End_Of_Line;
      Loop_Count := Loop_Count + 1;
      if Loop_Count > Ident_Int (3) then
         Failed ("END_OF_LINE ON DEFAULT INCORRECT - 6");
         exit;
      end if;
   end loop;

   Get (Char);
   if Char /= 'C' then
      Failed ("FINAL CHAR READ FROM FILE HAS WRONG VALUE - 7");
   end if;

   begin
      Delete (My_File);
   exception
      when others =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3404b;

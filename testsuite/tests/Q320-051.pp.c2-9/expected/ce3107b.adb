-- CE3107B.ADA

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
--     CHECK THAT IS_OPEN RETURNS THE PROPER VALUES FOR FILES OF
--     TYPE TEXT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH OUT_FILE MODE FOR TEXT FILES.

-- HISTORY:
--     DWC 08/17/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3107b is

   Test_File_One : File_Type;
   Test_File_Two : File_Type;
   Val           : Boolean;

   Incomplete : exception;

begin

   Test
     ("CE3107B",
      "CHECK THAT IS_OPEN RETURNS THE " &
      "PROPER VALUES FOR FILES OF TYPE TEXT_IO");

-- FOLLOWING A CREATE

   begin
      Val := False;
      Create (Test_File_One, Out_File, Legal_File_Name);
      Val := Is_Open (Test_File_One);
      if Val = False then
         Failed ("IS_OPEN RETURNS FALSE AFTER CREATE");
      end if;
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
   end;

-- FOLLOWING CLOSE

   Val := True;
   if Is_Open (Test_File_One) = True then
      Close (Test_File_One);
   end if;
   Val := Is_Open (Test_File_One);
   if Val = True then
      Failed ("IS_OPEN RETURNS TRUE AFTER CLOSE");
   end if;

-- FOLLOWING OPEN

   begin
      Val := False;
      begin
         Open (Test_File_Two, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            if Is_Open (Test_File_Two) /= False then
               Failed ("FILE OPEN AFTER USE_ERROR " & "DURING OPEN");
            end if;
            raise Incomplete;
      end;
      Val := Is_Open (Test_File_Two);
      if Val = False then
         Failed ("IS_OPEN RETURNS FALSE AFTER OPEN");
      end if;

-- AFTER RESET

      begin
         Val := False;
         Reset (Test_File_Two);
         Val := Is_Open (Test_File_Two);
         if Val = False then
            Failed ("IS_OPEN RETURNS FALSE AFTER RESET");
         end if;
      exception
         when Use_Error =>
            Comment ("IMPLEMENTATION DOES NOT SUPPORT RESET");
      end;
   exception
      when Incomplete =>
         null;
   end;

-- AFTER DELETE

   begin
      Val := True;
      Delete (Test_File_Two);
      Val := Is_Open (Test_File_Two);
      if Val = True then
         Failed ("IS_OPEN RETURNS TRUE AFTER DELETE");
      end if;
   exception
      when Use_Error =>
         if Is_Open (Test_File_Two) /= False then
            Failed ("FILE OPEN AFTER USE_ERROR " & "DURING DELETE");
         end if;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3107b;

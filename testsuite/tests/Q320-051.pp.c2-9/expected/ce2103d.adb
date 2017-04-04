-- CE2103D.ADA

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
--     TYPE DIRECT_IO.

--          B) OPENED FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTAIONS WHICH SUPPORT
--     CREATION OF EXTERNAL FILES FOR DIRECT FILES.

-- HISTORY:
--     SPW 08/13/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Direct_Io;

procedure Ce2103d is

   package Dir_Io is new Direct_Io (Character);
   use Dir_Io;
   Incomplete : exception;
   Test_File_One : Dir_Io.File_Type;
   Val           : Boolean;

begin

   Test
     ("CE2103D",
      "CHECK THAT IS_OPEN RETURNS THE PROPER " &
      "VALUES FOR FILES OF TYPE DIRECT_IO");

-- FOLLOWING A CREATE

   Val := False;

   begin
      Create (Test_File_One, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON CREATE WITH " & "OUT_FILE MODE");
         raise Incomplete;
   end;

   Val := Is_Open (Test_File_One);
   if Val = False then
      Failed ("IS_OPEN RETURNS FALSE AFTER CREATE");
   end if;

-- FOLLOWING CLOSE

   Val := True;
   Close (Test_File_One);
   Val := Is_Open (Test_File_One);
   if Val = True then
      Failed ("IS_OPEN RETURNS TRUE AFTER CLOSE");
   end if;

-- FOLLOWING OPEN

   Val := False;

   begin
      Open (Test_File_One, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         if Is_Open (Test_File_One) /= False then
            Failed ("IS_OPEN GIVES TRUE ON " & "UNSUCCESSFUL OPEN");
         end if;
         raise Incomplete;
   end;

   Val := Is_Open (Test_File_One);
   if Val = False then
      Failed ("IS_OPEN RETURNS FALSE AFTER OPEN");
   end if;

-- AFTER RESET

   Val := False;

   begin
      Reset (Test_File_One);
   exception
      when Use_Error =>
         null;
   end;

   Val := Is_Open (Test_File_One);
   if Val = False then
      Failed ("IS_OPEN RETURNS FALSE AFTER RESET");
   end if;

-- AFTER DELETE

   Val := True;

   begin
      Delete (Test_File_One);
   exception
      when Use_Error =>
         if Is_Open (Test_File_One) /= False then
            Failed ("IS_OPEN GIVES TRUE ON UNSUCCESSFUL " & "DELETE");
         end if;
         raise Incomplete;
   end;

   Val := Is_Open (Test_File_One);
   if Val = True then
      Failed ("IS_OPEN RETURNS TRUE AFTER DELETE");
   end if;

   Result;

exception

   when Incomplete =>
      Result;

end Ce2103d;

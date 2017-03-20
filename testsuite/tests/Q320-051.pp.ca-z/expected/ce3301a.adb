-- CE3301A.ADA

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
--     CHECK THAT WHEN THE LINE AND PAGE LENGTH ARE NONZERO, LINE AND
--     PAGE TERMINATORS ARE OUTPUT AT THE APPROPRIATE POINTS.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/22/82
--     SPS 11/15/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/27/87  COMPLETELY REVISED TEST.
--     LDC 05/26/88  ADDED "FILE" PARAMETERS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3301a is

   Incomplete : exception;
   File  : File_Type;
   Two   : constant Count := Count (Ident_Int (2));
   Ten   : constant Count := Count (Ident_Int (10));
   Three : constant Count := Count (Ident_Int (3));
   Item1 : String (1 .. 10);
   Item2 : String (1 .. 2);

begin

   Test
     ("CE3301A",
      "CHECK THAT WHEN THE LINE AND PAGE LENGTH ARE " &
      "NONZERO, LINE AND PAGE TERMINATORS ARE " &
      "OUTPUT AT THE APPROPRIATE POINTS");

   begin
      Create (File, Out_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable
           ("NAME_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
         raise Incomplete;
   end;

   if Line_Length (File) /= Unbounded then
      Failed ("LINE LENGTH NOT INITIALLY UNBOUNDED");
   end if;

   if Page_Length (File) /= Unbounded then
      Failed ("PAGE LENGTH NOT INITIALLY UNBOUNDED");
   end if;

   Set_Line_Length (File, Ten);
   Set_Page_Length (File, Two);

   for I in 1 .. 30 loop
      Put (File, 'C');
   end loop;

   if Page (File) /= 2 and Line (File) /= 1 then
      Failed ("LINE AND PAGE LENGTHS WERE NOT BOUND " & "CORRECTLY");
   end if;

   Set_Line_Length (File, Two);
   Set_Page_Length (File, Three);
   Put (File, "DDDDDDD");

   Close (File);

   begin
      Open (File, In_File, Legal_File_Name);
   exception
      when Use_Error =>
         Not_Applicable
           ("USE_ERROR RAISED ON TEXT OPEN WITH " & "IN_FILE MODE");
         raise Incomplete;
   end;

   Get (File, Item1);

   if not (End_Of_Line (File)) then
      Failed ("INCORRECT VALUE BEFORE LINE TERMINATOR");
   end if;

   if End_Of_Page (File) then
      Failed ("PAGE TERMINATOR OUTPUT AT INAPPROPRIATE POINT");
   end if;

   Get (File, Item1);

   if Item1 /= "CCCCCCCCCC" then
      Failed ("INCORRECT VALUE READ");
   end if;

   if not (End_Of_Line (File)) then
      Failed ("INCORRECT VALUE BEFORE LINE TERMINATOR");
   end if;

   if not (End_Of_Page (File)) then
      Failed ("INCORRECT VALUE BEFORE PAGE TERMINATOR");
   end if;

   Get (File, Item1);
   Get (File, Item2);

   if Item2 /= "DD" then
      Failed ("INCORRECT VALUE READ");
   end if;

   if not (End_Of_Line (File)) then
      Failed ("INCORRECT VALUE BEFORE LINE TERMINATOR");
   end if;

   if End_Of_Page (File) then
      Failed ("PAGE TERMINATOR OUTPUT AT INAPPROPRIATE POINT");
   end if;

   Get (File, Item2);

   if Item2 /= "DD" then
      Failed ("INCORRECT VALUE READ");
   end if;

   if not (End_Of_Line (File)) then
      Failed ("INCORRECT VALUE BEFORE LINE TERMINATOR");
   end if;

   if not (End_Of_Page (File)) then
      Failed ("INCORRECT VALUE BEFORE PAGE TERMINATOR");
   end if;

   begin
      Delete (File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3301a;

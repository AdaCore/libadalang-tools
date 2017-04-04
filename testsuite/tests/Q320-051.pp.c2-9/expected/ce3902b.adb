-- CE3902B.ADA

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
--     CHECK THAT THE OPERATIONS IN GENERIC PACKAGE ENUMERATION_IO
--     ALL HAVE THE CORRECT PARAMETER NAMES.

-- HISTORY:
--     JLH 08/25/88  CREATED ORIGINAL TEST.
--     RJW 02/28/90  ADDED CODE TO PREVENT MODE_ERROR FROM BEING RAISED.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3902b is

   type Color is (Red, Blue, Green);
   package Color_Io is new Enumeration_Io (Color);
   use Color_Io;

   File1        : File_Type;
   Crayon       : Color := Red;
   Index        : Positive;
   Num          : Field := 5;
   Color_String : String (1 .. 5);
   Incomplete : exception;

begin

   Test
     ("CE3902B",
      "CHECK THAT THE OPERATIONS IN GENERIC PACKAGE " &
      "ENUMERATION_IO ALL HAVE THE CORRECT PARAMETER " &
      "NAMES");

   begin

      begin
         Create (File1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Set_Output (File1);

      Put (File => File1, Item => Crayon, Width => Num, Set => Upper_Case);

      Put (Item => Green, Width => 5, Set => Lower_Case);

      Put (To => Color_String, Item => Blue, Set => Upper_Case);

      Close (File1);

      Set_Output (Standard_Output);

      begin
         Open (File1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON OPEN WITH " & "MODE IN_FILE");
            raise Incomplete;
      end;

      Set_Input (File1);

      Get (File => File1, Item => Crayon);

      Get (Item => Crayon);

      Get (From => Color_String, Item => Crayon, Last => Index);

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3902b;

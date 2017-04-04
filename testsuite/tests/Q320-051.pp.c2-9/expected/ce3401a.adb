-- CE3401A.ADA

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
--     CHECK THAT THE FORMAL PARAMETERS OF EACH COLUMN, LINE, AND
--     PAGE OPERATION ARE NAMED CORRECTLY.

-- HISTORY:
--     JET 08/17/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;
procedure Ce3401a is

   Fin, Fout : File_Type;
   B         : Boolean;
   C         : Count;
   File_Ok   : Boolean := False;

begin
   Test
     ("CE3401A",
      "CHECK THAT THE FORMAL PARAMETERS OF EACH " &
      "COLUMN, LINE, AND PAGE OPERATION ARE NAMED " &
      "CORRECTLY");

   begin
      Create (Fout, Out_File, Legal_File_Name);
      File_Ok := True;
   exception
      when others =>
         Not_Applicable ("OUTPUT FILE COULD NOT BE CREATED");
   end;

   if File_Ok then
      New_Line (File => Fout, Spacing => 1);
      New_Page (File => Fout);
      Set_Col (File => Fout, To => 1);
      Set_Line (File => Fout, To => 1);
      C := Col (File => Fout);
      C := Line (File => Fout);
      C := Page (File => Fout);

      New_Page (Fout);

      begin
         Close (Fout);
      exception
         when others =>
            Failed ("OUTPUT FILE COULD NOT BE CLOSED");
            File_Ok := False;
      end;
   end if;

   if File_Ok then
      begin
         Open (Fin, In_File, Legal_File_Name);
      exception
         when others =>
            Failed ("INPUT FILE COULD NOT BE OPENED");
            File_Ok := False;
      end;
   end if;

   if File_Ok then
      Skip_Line (File => Fin, Spacing => 1);
      Skip_Page (File => Fin);
      B := End_Of_Line (File => Fin);
      B := End_Of_Page (File => Fin);
      B := End_Of_File (File => Fin);

      begin
         Delete (Fin);
      exception
         when Use_Error =>
            Comment ("FILE COULD NOT BE DELETED");
         when others =>
            Failed ("UNEXPECTED ERROR AT DELETION");
      end;
   end if;

   Result;
exception
   when others =>
      Failed ("UNEXPECTED EXCEPTION RAISED");
end Ce3401a;

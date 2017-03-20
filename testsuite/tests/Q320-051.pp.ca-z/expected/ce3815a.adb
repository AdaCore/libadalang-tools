-- CE3815A.ADA

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
--     CHECK THAT THE OPERATIONS IN GENERIC PACKAGE FLOAT_IO ALL HAVE
--     THE CORRECT PARAMETER NAMES.

-- HISTORY:
--     JET 10/28/88  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;
procedure Ce3815a is

   Str       : String (1 .. 20) := (others => ' ');
   Fin, Fout : File_Type;
   F         : Float;
   L         : Positive;
   File_Ok   : Boolean          := False;

   package Fio is new Float_Io (Float);
   use Fio;

begin
   Test
     ("CE3815A",
      "CHECK THAT THE OPERATIONS IN GENERIC PACKAGE " &
      "FLOAT_IO ALL HAVE THE CORRECT PARAMETER NAMES");

   Put (To => Str, Item => 1.0, Aft => 3, Exp => 3);
   Get (From => Str, Item => F, Last => L);

   begin
      Create (Fout, Out_File, Legal_File_Name);
      File_Ok := True;
   exception
      when others =>
         Comment ("OUTPUT FILE COULD NOT BE CREATED");
   end;

   if File_Ok then
      begin
         Put (File => Fout, Item => 1.0, Fore => 3, Aft => 3, Exp => 3);
         New_Line (Fout);

         Close (Fout);
      exception
         when others =>
            Failed ("OUTPUT FILE COULD NOT BE WRITTEN");
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
      begin
         Get (File => Fin, Item => F, Width => 10);
      exception
         when others =>
            Failed ("DATA COULD NOT BE READ FROM FILE");
      end;

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
end Ce3815a;

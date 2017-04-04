-- CE3904A.ADA

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
--     CHECK THAT THE LAST NONBLANK CHARACTER IN A FILE MAY BE READ BY
--     'GET' IN ENUMERATION_IO WITHOUT RAISING END_ERROR, AND THAT AFTER
--     THE LAST CHARACTER OF THE FILE HAS BEEN READ, ANY ATTEMPT TO READ
--     FURTHER CHARACTERS WILL RAISE END_ERROR.

-- HISTORY:
--     JET 08/19/88  CREATED ORIGINAL TEST.

with Report, Text_Io;
use Report, Text_Io;
procedure Ce3904a is

   type Enum is (The, Quick, Brown, X);
   E : Enum;

   package Eio is new Enumeration_Io (Enum);
   use Eio;

   F : File_Type;

   File_Ok : Boolean := False;

begin
   Test
     ("CE3904A",
      "CHECK THAT THE LAST NONBLANK CHARACTER IN A " &
      "FILE MAY BE READ BY 'GET' IN ENUMERATION_IO " &
      "WITHOUT RAISING END_ERROR, AND THAT AFTER THE " &
      "LAST CHARACTER OF THE FILE HAS BEEN READ, ANY " &
      "ATTEMPT TO READ FURTHER CHARACTERS WILL RAISE " &
      "END_ERROR");

   begin
      Create (F, Out_File, Legal_File_Name);
      File_Ok := True;
   exception
      when others =>
         Not_Applicable ("DATA FILE COULD NOT BE OPENED FOR " & "WRITING");
   end;

   if File_Ok then
      begin
         Put (F, The);
         New_Line (F);
         Put (F, Quick);
         New_Line (F);
         Put (F, Brown);
         New_Line (F);
         Put (F, X);
         New_Line (F);
         Close (F);
      exception
         when others =>
            Not_Applicable ("DATA FILE COULD NOT BE WRITTEN");
            File_Ok := False;
      end;
   end if;

   if File_Ok then
      begin
         Open (F, In_File, Legal_File_Name);
         for I in 0 .. 3 loop
            Get (F, E);
            if E /= Enum'Val (I) then
               Failed ("INCORRECT VALUE READ -" & Integer'Image (I));
            end if;
         end loop;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED BEFORE END " & "OF FILE");
            File_Ok := False;
      end;
   end if;

   if File_Ok then
      begin
         Get (F, E);
         Failed ("NO EXCEPTION RAISED AFTER END OF FILE");
      exception
         when End_Error =>
            null;
         when others =>
            Failed ("INCORRECT EXCEPTION RAISED AFTER END OF " & "FILE");
      end;

      begin
         Delete (F);
      exception
         when others =>
            Comment ("DATA FILE COULD NOT BE DELETED");
      end;
   end if;

   Result;
end Ce3904a;

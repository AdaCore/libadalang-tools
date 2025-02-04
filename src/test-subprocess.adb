------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2014-2024, AdaCore                    --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with Test.Common;                use Test.Common;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
with Utils_Debug;
with Utils.Environment;

with GNATCOLL.OS.FS;

package body Test.Subprocess is

   ------------
   -- PP_Cmd --
   ------------

   procedure PP_Cmd (Cmd : Argument_List; Prefix : String := "") is
      Result : Unbounded_String;
   begin
      for Arg of Cmd loop
         Result := Result & Arg & " ";
      end loop;
      Report_Err
        ((if Prefix /= "" then Prefix & " " else "") & To_String (Result));
   end PP_Cmd;

   ---------
   -- Run --
   ---------

   procedure Run
     (Cmd : Argument_List; What : String := ""; Out_To_Null : Boolean := False)
   is
      use GNATCOLL.OS.FS;
      Return_Status : Integer;
      Out_FD        : constant File_Descriptor :=
        (if Out_To_Null then Null_FD else Standout);
   begin
      if Utils_Debug.Debug_Flag_2
        or else Utils_Debug.Debug_Flag_1
        or else Test.Common.Verbose
      then
         PP_Cmd (Cmd, "Running");
      end if;
      Return_Status := Run (Cmd, Stdout => Out_FD, Stderr => Out_FD);
      if Return_Status /= 0 then
         Report_Err
           ((if What = "" then Cmd.First_Element else What) & " failed.");
         PP_Cmd (Cmd, "Command was: ");
         Utils.Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   exception
      when Exc : GNATCOLL.OS.OS_Error =>
         PP_Cmd (Cmd, "Exception raised while running the following command:");
         if Test.Common.Verbose then
            Report_Err (Ada.Exceptions.Exception_Information (Exc));
         else
            Report_Err
              (Ada.Exceptions.Exception_Name (Exc)
               & ": "
               & Ada.Exceptions.Exception_Message (Exc));
         end if;
         Utils.Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
   end Run;

   ---------------------
   -- Populate_X_Vars --
   ---------------------

   procedure Populate_X_Vars
     (Cmd : in out Argument_List; Gnattest_Cmd : Command_Line)
   is
      use Common_String_Seq_Switches;
      Ext_Vars : constant String_Ref_Array :=
        Arg (Gnattest_Cmd, External_Variable);
   begin
      for Var of Ext_Vars loop
         if Present (Var) then
            Cmd.Append ("-X" & Var.all);
         end if;
      end loop;
   end Populate_X_Vars;

end Test.Subprocess;

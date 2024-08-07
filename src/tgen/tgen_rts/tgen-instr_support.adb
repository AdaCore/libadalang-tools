------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body TGen.Instr_Support is

   Saved_Output_Dir : Unbounded_String;

   Output_Dir_Exists : Boolean := False;

   ----------------
   -- Output_Dir --
   ----------------

   function Output_Dir return String is
      Res : Unbounded_String := Saved_Output_Dir;
   begin
      --  Return a fixed default directory if not specified

      if Length (Res) = 0 then
         Res := To_Unbounded_String ("tgen_test_inputs");
      end if;

      --  The instrumentation process expects that the returned value already
      --  contains a directory separator at the end, so ensure this is the
      --  case.

      if Element (Res, Length (Res)) /= GNAT.OS_Lib.Directory_Separator then
         Res := Res & GNAT.OS_Lib.Directory_Separator;
      end if;

      --  Also create the directory if it does not exist yet

      if not Output_Dir_Exists then
         if not Ada.Directories.Exists (To_String (Res)) then
            Ada.Directories.Create_Path (To_String (Res));
         end if;
         Output_Dir_Exists := True;
      end if;

      return To_String (Res);
   exception
      --  Abort if something went wrong

      when others =>
         Ada.Text_IO.Put_Line
           ("gnattest: failed to create test output directory, aborting.");
         GNAT.OS_Lib.OS_Exit (1);
   end Output_Dir;

   --------------------
   -- Set_Output_Dir --
   --------------------

   procedure Set_Output_Dir (Dir : String) is
   begin
      Saved_Output_Dir := To_Unbounded_String (Dir);
      Output_Dir_Exists := False;
   end Set_Output_Dir;

end TGen.Instr_Support;

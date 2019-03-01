------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                  A S I S _ U L . E N V I R O N M E N T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2017, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

with Utils.Command_Lines; use Utils.Command_Lines;

package body Utils.Environment is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Temp_Dir_Parent return String;
   --  If non-empty, points to the name of the directory in which to create the
   --  tool's temporary directory. This comes from the value of the TMPDIR
   --  environment variable. If empty, the temporary directory is created in
   --  the current directory.

   function Get_Temp_Dir_Parent return String is
      Tmpdir : constant String := "TMPDIR";
      Dir    : String_Access   := Getenv (Tmpdir);
   begin
      if Dir.all /= "" and then Is_Absolute_Path (Dir.all)
        and then Is_Directory (Dir.all)
      then
         declare
            Result : constant String := Normalize_Pathname (Dir.all);
         begin
            Free (Dir);
            return Result;
         end;

      else
         Free (Dir);
         return "";
      end if;
   end Get_Temp_Dir_Parent;

   Temp_Dir_Parent : constant String := Get_Temp_Dir_Parent;

   function Temp_Dir_Exists return Boolean is (Tool_Temp_Dir /= null);
   --  True if the Tool_Temp_Dir has been created, but not yet deleted by
   --  Clean_Up. Note that we create and delete the Tool_Temp_Dir multiple
   --  times per run.

   --   Tmpdir_Displayed : Boolean := False;
   --  True if the value of the TMPDIR environment variable has been displayed;
   --  we don't want to display it more than once per run.

   --------------
      -- Clean_Up --
      --------------

   procedure Clean_Up is
      Success : Boolean := False;
   begin
      --  Clean up temporary dir

      if not Debug_Flag_N and then Temp_Dir_Exists then

         pragma Assert
           (Get_Current_Dir = Tool_Temp_Dir.all & Directory_Separator
            or else Get_Current_Dir = Initial_Dir & Directory_Separator);
         --  It can be Initial_Dir if we fail early
         Change_Dir (Initial_Dir);
         --  to avoid being in the Tool_Temp_Dir when we delete it

         for J in 1 .. 10 loop
            --  On windows, there might be a slight delay between the return of
            --  the close function on a file descriptor and the actual closing
            --  done by the system. Since it's not possible to remove a
            --  directory as long as there are handles on it, this Remove_Dir
            --  may fail. So, if a call to Remove_Dir raises Directory_Error,
            --  we try several times after some delay, and only if all the
            --  attempts fail, we generate an error message and raise an
            --  exception.

            begin
               Remove_Dir (Tool_Temp_Dir.all, Recursive => True);
               Success := True;
               exit;
            exception
               when Directory_Error =>
                  delay 0.05;
            end;

         end loop;

         if not Success then
            --  Because of some unknown reason the temporary directory cannot
            --  be removed:
            Free (Tool_Temp_Dir);  -- to avoid cycling
            Cmd_Error ("cannot remove temporary directory");
         end if;

         Free (Tool_Temp_Dir);

      end if;

   end Clean_Up;

   ---------------------
   -- Create_Temp_Dir --
   ---------------------

   procedure Create_Temp_Dir is
      FD        : File_Descriptor;
      Temp_Name : Temp_File_Name;
      Success   : Boolean;
   begin
      pragma Assert (Get_Current_Dir = Initial_Dir & Directory_Separator);
      pragma Assert (not Temp_Dir_Exists);

      if Temp_Dir_Parent /= "" then
         Change_Dir (Temp_Dir_Parent);
      --  So when we create the Tool_Temp_Dir below, it will be a
      --  subdirectory of Temp_Dir_Parent.
      end if;

      --  ??? We create the temp dir by first creating the temp file, then
      --  closing and deleting it, then creating a dir with the same name.
      --  This is not atomic as another program can sneak in between file
      --  deletion and dir creation and snatch this name for itself. This is
      --  quite unlikely and anyway we don't have any other system-independent
      --  way at the moment
      --  ????????????????I think Utils is now using a better method.
      Create_Temp_File (FD, Temp_Name);
      if FD = Invalid_FD then
         Cmd_Error ("cannot create temp file; directory is not writeable");
      end if;
      Close (FD);
      Delete_File (Temp_Name, Success);

      if not Success then
         Cmd_Error ("cannot delete temp file " & Temp_Name);
      end if;

      Tool_Temp_Dir :=
        new String' -- Remove NUL

          (Normalize_Pathname
             (Temp_Name (Temp_Name'First .. Temp_Name'Last - 1)));

      Parallel_Make_Dir (Tool_Temp_Dir.all);

      Change_Dir (Initial_Dir);
   exception
      when Directory_Error =>
         Cmd_Error ("cannot create the temp directory " & Tool_Temp_Dir.all);
   end Create_Temp_Dir;

   -------------------
   -- Copy_Gnat_Adc --
   -------------------

   procedure Copy_Gnat_Adc is
      Success : Boolean;
   begin
      if Is_Regular_File ("gnat.adc") then
         Copy_File
           (Name => Tool_Current_Dir.all & Directory_Separator & "gnat.adc",
            Pathname => Tool_Temp_Dir.all & Directory_Separator & "gnat.adc",
            Success  => Success, Mode => Copy);
      end if;
   end Copy_Gnat_Adc;

end Utils.Environment;

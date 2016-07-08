------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                  A S I S _ U L . E N V I R O N M E N T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2016, AdaCore                      --
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

pragma Ada_2012;

--  with Ada.Directories;             use Ada.Directories;
--  with Ada.Text_IO;                 use Ada.Text_IO;
--  with System.Multiprocessors;

--  with LAL_UL.Formatted_Output;

--  with LAL_UL.Tool_Names; use LAL_UL.Tool_Names;
with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;

package body LAL_UL.Environment is

   -----------------------
   -- Local subprograms --
   -----------------------

   pragma Warnings (Off); -- ????????????????
   function Builder_Command_Line return Argument_List;
   pragma Warnings (On);
   --  Called by the outer invocation of an ASIS tool when in Incremental_Mode.
   --  Returns a sequence of command-line arguments suitable for invoking the
   --  builder. See also the comments on ASIS_UL.Options.Incremental_Mode.

   function Get_Temp_Dir_Parent return String;
   --  If non-empty, points to the name of the directory in which to create the
   --  tool's temporary directory. This comes from the value of the TMPDIR
   --  environment variable. If empty, the temporary directory is created in
   --  the current directory.

   function Get_Temp_Dir_Parent return String is
      Tmpdir    : constant String := "TMPDIR";
      Dir : String_Access := Getenv (Tmpdir);
   begin
      if Dir.all /= "" and then
        Is_Absolute_Path (Dir.all) and then
        Is_Directory (Dir.all)
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

   function Temp_Dir_Exists return Boolean is
      (Tool_Temp_Dir /= null);
   --  True if the Tool_Temp_Dir has been created, but not yet deleted by
   --  Clean_Up. Note that we create and delete the Tool_Temp_Dir multiple
   --  times per run.

--   Tmpdir_Displayed : Boolean := False;
   --  True if the value of the TMPDIR environment variable has been displayed;
   --  we don't want to display it more than once per run.

   --------------------------
   -- Builder_Command_Line --
   --------------------------

   pragma Style_Checks (Off); -- ????????????????
   function Builder_Command_Line return Argument_List is
--      pragma Assert (Incremental_Mode);
--
--      Builder_Args, Inner_Args : String_Vector;
--      --  Builder_Args are passed to the builder. Inner_Args are passed to the
--      --  inner invocations of the ASIS tool by passing them to the builder
--      --  after "-cargs".
--      Cur : Positive := 1;
--      In_Gnatcheck_Rules : Boolean := False;
--      --  True if the loop below is in the "-rules" section
--
--      This_Is_Gnatcheck : constant Boolean :=
--        Has_Suffix (Tool_Name, Suffix => "gnatcheck");
--      --  Flag for special-casing gnatcheck. "Suffix" instead of "=" in case
--      --  it's a cross compiler.
--
--      use Ada.Command_Line;
--
   begin
      return (1 .. 0 => <>);

--      --  Tell the builder to keep quiet
--
--      if Debug_Flag_C then
--         Append (Builder_Args, "-v");
--      else
--         Append (Builder_Args, "-q");
--      end if;
--
--      --  Tell the builder to pretend that the ASIS tool is the compiler, and
--      --  which project-file package to use.
--
--      Append (Builder_Args,
--              String'("--compiler-subst=ada," &
--                        Ada.Command_Line.Command_Name));
--
--      declare
--         Pkg_Name : constant String :=
--           --  Package name (in project file) corresponding to the
--           --  tool. Empty string if none.
--           (if Has_Suffix (Tool_Name, Suffix => "gnat2xml")
--              then "gnat2xml"
--            elsif Has_Suffix (Tool_Name, Suffix => "gnatpp")
--              then "pretty_printer"
--            elsif Has_Suffix (Tool_Name, Suffix => "gnatcheck")
--              then "check"
--            elsif Has_Suffix (Tool_Name, Suffix => "gnatmetric")
--              then "metrics"
--            else raise Program_Error); -- other tools don't have --incremental
--      begin
--         Append (Builder_Args,
--                 String'("--compiler-pkg-subst=" & Pkg_Name));
--      end;
--
--      --  Tell the builder to create necessary directories if they don't exist
--
--      Append (Builder_Args, "-p");
--
--      --  Tell the builder not to complain about missing object files. We are
--      --  pretending that the ASIS tool is the compiler, but of course ASIS
--      --  tools don't generate object files.
--
--      Append (Builder_Args, "--no-object-check");
--
--      --  Compile only
--
--      Append (Builder_Args, "-c");
--
--      --  Tell the builder to place ALI files in a subdirectory of the object
--      --  directory -- a different subdirectory for each tool. This is
--      --  necessary because otherwise the ALI files would conflict with each
--      --  other. For one thing, ASIS tools run the compiler (the real one) with
--      --  different switches than normal builds, so we don't want to overwrite
--      --  one kind of ALI file with the other. For another thing, just because
--      --  the files generated by gnat2xml are up to date doesn't mean that the
--      --  files generated by gnatcheck are up to date. So if the object
--      --  directory is 'obj', normal builds put ALI files in obj, "gnat2xml
--      --  --incremental" puts ALI files in obj/ALI-FILES-gnat2xml, and so on.
--
--      Append (Builder_Args,
--              String'("--subdirs=" & "ALI-FILES-" & Tool_Name));
--
--      --  Don't bother with code in other languages.
--
--      Append (Builder_Args, "--restricted-to-languages=ada");
--
--      --  Recompile if switches have changed
--
--      Append (Builder_Args, "-s");
--
--      --  If "-files=f" was given, append all the file names from f. It might
--      --  be better to teach the builder how to use -files= directly, to avoid
--      --  command-line length limitations.
--
--      for F of Files_From_File loop
--         Append (Builder_Args, F);
--      end loop;
--
--      --  Inform the inner invocation where to find input files.
--
--      Append (Inner_Args, String'("--outer-dir=" & Tool_Current_Dir.all));
--
--      --  Modify the --output-dir= switch so it uses a full pathname.
--      --  Otherwise, the output files would end up in something like
--      --  obj/ALI-FILES-gnat2xml.
--
--      if Out_Dir /= null then
--         Append (Inner_Args,
--                 String'("--output-dir=" & Out_Dir.all));
--      end if;
--
--      --  Set the report file name for the inner invocation using a full path
--      --  name, because sometimes the inner invocation has a different current
--      --  directory.
--
--      if This_Is_Gnatcheck then
--         if Text_Report_ON then
--            Append (Inner_Args, "-o");
--            Append (Inner_Args, Get_Report_File_Name);
--         end if;
--
--         if XML_Report_ON then
--            Append (Inner_Args, "-ox");
--            Append (Inner_Args, Get_XML_Report_File_Name);
--         end if;
--      end if;
--
--      --  Now deal with command-line arguments from this invocation of an ASIS
--      --  tool (the outer one). Most are copied to Builder_Args or Inner_Args.
--
--      while Cur <= Argument_Count loop
--         declare
--            Arg : constant String := Argument (Cur);
--
--            function Match
--              (Check : String;
--               Kind : Character := ' ';
--               Builder_Arg, Inner_Arg : Boolean := False) return Boolean
--              with Pre => Kind in ' ' | ':' | '=' | '!';
--            --  Checks if Arg matches Check. Kind = ' ' means Arg does not have
--            --  a parameter. The other possibilities for Kind mean the same
--            --  thing as in GNAT.Command_Line.Getopt. If there is a match, then
--            --  we move past the arg and its parameter, if any, and append them
--            --  onto Builder_Args and/or Inner_Args, as indicated by the
--            --  Builder_Arg and Inner_Arg flags. Return True iff there is a
--            --  match. To ignore an argument, leave Builder_Arg and Inner_Arg
--            --  False.
--
--            function Match
--              (Check : String;
--               Kind : Character := ' ';
--               Builder_Arg, Inner_Arg : Boolean := False) return Boolean is
--
--               procedure App;
--               procedure App is
--               begin
--                  if Builder_Arg then
--                     Append (Builder_Args, Argument (Cur));
--                  end if;
--                  if Inner_Arg then
--                     Append (Inner_Args, Argument (Cur));
--                  end if;
--               end App;
--
--               Old_Cur : constant Positive := Cur;
--            begin
--
--               case Kind is
--                  when ' ' =>
--                     if Arg = Check then
--                        App;
--                        Cur := Cur + 1;
--                     end if;
--
--                  when ':' =>
--                     if Arg = Check then
--                        App;
--                        Cur := Cur + 1;
--                        App;
--                        Cur := Cur + 1;
--                     elsif Has_Prefix (Arg, Prefix => Check) then
--                        App;
--                        Cur := Cur + 1;
--                     end if;
--
--                  when '=' =>
--                     if Arg = Check then
--                        App;
--                        Cur := Cur + 1;
--                        App;
--                        Cur := Cur + 1;
--                     elsif Has_Prefix (Arg, Prefix => Check & "=") then
--                        App;
--                        Cur := Cur + 1;
--                     end if;
--
--                  when '!' =>
--                     if Has_Prefix (Arg, Prefix => Check) then
--                        App;
--                        Cur := Cur + 1;
--                     end if;
--
--                  when others => raise Program_Error;
--               end case;
--
--               return Cur /= Old_Cur;
--            end Match;
--
--         begin
--            --  We shouldn't be seeing -c, -gnatc, -gnatec, -gnatem, or -gnateO
--            --  here; those are passed by gnatmake or gprbuild to the inner
--            --  invocations, whereas we're in the outer one. Might as well
--            --  ignore them. "--incremental" needs to be ignored; the inner
--            --  invocations shouldn't get here.  "--output-dir" is handled
--            --  specially above. Project-related arguments go in
--            --  Builder_Args. Non-switches (i.e. file names) go in
--            --  Builder_Args. Anything else goes in Inner_Args.
--            --
--            --  We ignore -files because that's covered by Files_From_File
--            --  above.
--
--            if Match ("-c", ' ')
--              or else Match ("-xml", ' ', Inner_Arg => True)
--              or else Match ("-gnatc", '!')
--              or else Match ("-gnatec", '!')
--              or else Match ("-gnatem", '!')
--              or else Match ("-gnateO", '!')
--              or else Match ("--incremental", ' ')
--              or else Match ("--output-dir", '=')
--              or else Match ("-files", '=')
--
--              or else Match ("-P", ':', Builder_Arg => True)
--              or else Match ("-U", ' ', Builder_Arg => True)
--              or else Match ("-X", '!', Builder_Arg => True)
--              or else Match ("--subdirs", '=', Builder_Arg => True)
--
--              or else Match ("--no_objects_dir", ' ', Inner_Arg => True)
--              or else Match ("-o", '=', Inner_Arg => True)
--            then
--               null; -- One of those Matches already Appended if appropriate
--            elsif Match ("-j", '!', Builder_Arg => True) then
--               --  The Match already Appended to Builder_Args, but we also want
--               --  to pass --outer-parallel to the inner invocation.
--               Append (Inner_Args, "--outer-parallel");
--            elsif not Has_Prefix (Arg, Prefix => "-") then
--               --  If it doesn't look like a switch (e.g. a source file name),
--               --  we want to pass it to the builder, except that gnatcheck
--               --  rules like "+Blah", should go to the inner tool invocation.
--               if In_Gnatcheck_Rules then
--                  Append (Inner_Args, Arg);
--               else
--                  Append (Builder_Args, Arg);
--               end if;
--               Cur := Cur + 1;
--            elsif Arg = "-cargs" then
--               --  We can't just pass -cargs, because it would be hi-jacked by
--               --  the builder. So we pass -inner-cargs instead. The inner
--               --  invocation will then use this different arg when it
--               --  processes Process_cargs_Section. So for example if our
--               --  (outer) args are:
--               --
--               --      some_file.adb -cargs -gnat2012
--               --
--               --  we will pass:
--               --
--               --      ... -cargs some_file.adb -inner-cargs -gnat2012 ...
--               --
--               --  to the builder, which will then pass:
--               --
--               --      ... some_file.adb -inner-cargs -gnat2012 ...
--               --
--               --  to the inner invocation.
--
--               Append (Inner_Args, "-inner-cargs");
--               Cur := Cur + 1;
--            else
--               pragma Assert (Has_Prefix (Arg, Prefix => "-"));
--               --  Here for all other switches, including -d (debug switches).
--               --  Pass the switch along to the inner invocation. In addition,
--               --  pass -dn (keep temp files) along to the builder.
--               Append (Inner_Args, Arg);
--               if Arg in "-dn" | "-debugn" then
--                  Append (Builder_Args, "-dn");
--               end if;
--               Cur := Cur + 1;
--
--               if This_Is_Gnatcheck and then Arg = "-rules" then
--                  In_Gnatcheck_Rules := True;
--               end if;
--            end if;
--         end;
--      end loop;
--
--      --  Include extra args specific to the ASIS tool
--
--      Append (Inner_Args, Extra_Inner_Post_Args);
--      Prepend (Inner_Args, Extra_Inner_Pre_Args);
--
--      --  -cargs means to pass the following arguments along to the ASIS tool
--
--      if Last_Index (Inner_Args) > 0 then
--         Prepend (Inner_Args, "-cargs:Ada");
--      end if;
--
--      --  Finally, construct the result, which is basically
--      --  "Builder_Args & Inner_Args".
--
--      return Result : Argument_List
--        (1 .. Last_Index (Builder_Args) + Last_Index (Inner_Args))
--      do
--         declare
--            X : Natural := 0;
--         begin
--            for Arg of Builder_Args loop
--               X := X + 1;
--               Result (X) := new String'(Arg);
--            end loop;
--
--            for Arg of Inner_Args loop
--               X := X + 1;
--               Result (X) := new String'(Arg);
--            end loop;
--
--            pragma Assert (X = Result'Last);
--         end;
--      end return;
   end Builder_Command_Line;

   ------------------
   -- Call_Builder --
   ------------------

   procedure Call_Builder is
   begin
      null;
--      if not A4G.GNAT_Int.Execute
--        (ASIS_UL.Common.Gprbuild_To_Call,
--         Builder_Command_Line,
--         Display_Call => ASIS_UL.Debug.Debug_Flag_C)
--      then
--         raise ASIS_UL.Common.Fatal_Error;
--         --  Presumably the builder or one of the inner invocations printed an
--         --  error message.
--      end if;
   end Call_Builder;

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
      --  ????????????????I think ASIS_UL is now using a better method.
      Create_Temp_File (FD, Temp_Name);
      Close (FD);
      Delete_File (Temp_Name, Success);

      if not Success then
         Cmd_Error ("can not delete the temporary file that was just created");
      end if;

      Tool_Temp_Dir := new String' -- Remove NUL
        (Normalize_Pathname
           (Temp_Name (Temp_Name'First .. Temp_Name'Last - 1)));

      Parallel_Make_Dir (Tool_Temp_Dir.all);

      Change_Dir (Initial_Dir);

   exception
      when Directory_Error =>
         Cmd_Error ("cannot create the temporary directory");
   end Create_Temp_Dir;

   -------------------
   -- Copy_Gnat_Adc --
   -------------------

   procedure Copy_Gnat_Adc is
      Success : Boolean;
   begin
      if Is_Regular_File ("gnat.adc") then
         Copy_File
           (Name     => Tool_Current_Dir.all & Directory_Separator &
                        "gnat.adc",
            Pathname => Tool_Temp_Dir.all & Directory_Separator &
                        "gnat.adc",
            Success  => Success,
            Mode     => Copy);
      end if;
   end Copy_Gnat_Adc;

end LAL_UL.Environment;

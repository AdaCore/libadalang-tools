------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

with System.WCh_Con;

package Utils.Command_Lines.Common is

   Common_Descriptor : Command_Line_Descriptor;

   type Common_Flags is
     (Version,
      Help,
      Verbose,
      Quiet,
      Compile_Switch,
      Avoid_Processing_Gnat_Adc,
      No_Code_Generation,
      Incremental,
      Outer_Parallel,
      Rep_Clauses,
      Follow_Symbolic_Links,
      No_Objects_Dir,
      Compute_Timing,
      Process_RTL_Units,
      Cargs);

   package Common_Flag_Switches is new Flag_Switches
     (Common_Descriptor,
      Common_Flags);

   package Common_Flag_Shorthands is new Common_Flag_Switches.Set_Shorthands
     ([Version | Help => null,
       Verbose                   => +"-v",
       Quiet                     => +"-q",
       Compile_Switch            => +"-c", -- ignored
       Avoid_Processing_Gnat_Adc => +"-gnatA",
       No_Code_Generation        => +"-gnatc", -- ignored
       Incremental               => null,
       Outer_Parallel            => null,
       Rep_Clauses               => null,
       Follow_Symbolic_Links     => +"-eL",
       No_Objects_Dir            => null,
       Compute_Timing            => +"-t",
       Process_RTL_Units         => +"-a",
       Cargs                     => +"-cargs"]);

   type Common_Booleans is (Syntax_Only);

   package Common_Boolean_Switches is new Boolean_Switches
     (Common_Descriptor,
      Common_Booleans);

   package Common_Boolean_Defaults is new
     Common_Boolean_Switches.Set_Defaults
       ([Syntax_Only => False]);

   package Common_Boolean_Shorthands is new
     Common_Boolean_Switches.Set_Shorthands
       ([Syntax_Only => +"-so"]);

   type Ada_Version_Type is (Ada_83, Ada_95, Ada_2005, Ada_2012);

   package Ada_Version_Switches is new Enum_Switches
     (Common_Descriptor, Ada_Version_Type,
      Default => Ada_Version_Type'Last);
   --  These switches are ignored. The tools are tolerant of using newer
   --  reserved words, such as "interface", as identifiers, so we don't need to
   --  know the version.

   package Ada_Version_Shorthands is new Ada_Version_Switches.Set_Shorthands
     ([Ada_83 => +"-gnat83",
       Ada_95 => +"-gnat95",
       Ada_2005 => +"-gnat2005",
       Ada_2012 => +"-gnat2012"]);

   package Ada_Version_Shorthands_2 is new Ada_Version_Switches.Set_Shorthands
     ([Ada_2005 => +"-gnat05",
       others => null]);

   type Source_Selection_Type is
     (Update_All, No_Subprojects, No_Source_Selection);

   package Source_Selection_Switches is new Enum_Switches
     (Common_Descriptor, Source_Selection_Type,
      Default => Source_Selection_Type'Last);

   package Source_Selection_Shorthands is new
     Source_Selection_Switches.Set_Shorthands
       ([Update_All => +"-U",
         others => null]);

   type Common_Strings is
     (Project_File,
      Aggregated_Project_File,
      Run_Time_System,
      Configuration_Pragmas_File,
      Mapping_File,
      Object_Path_File_Name,
      Include_Path,
   --  ????????????????Need to deal with "-x ada". See "when 'x' =>" in
   --  Utils.Environment.Scan_Common_Arg for details. Perhaps we could put
   --  a kludge in Text_Args_From_Command_Line.
      Outer_Dir,
      Output_Directory,
      Target,
      Subdirs,
      Wide_Character_Encoding -- Use Enum_Switches????
     );

   --  Aggregated_Project_File is an undocumented switch used in the
   --  implementation of aggregate projects. When gnatpp is invoked with an
   --  aggregate project where the aggregated projects are a, b, and c, it
   --  spawns 3 subprocesses passing --aggregated-project-file=a,
   --  --aggregated-project-file=b, and  --aggregated-project-file=c.

   package Common_String_Switches is new String_Switches
     (Common_Descriptor,
      Common_Strings);

   package Common_String_Syntax is new Common_String_Switches.Set_Syntax
     ([Project_File               => ':',
       Aggregated_Project_File    => '=',
       Run_Time_System            => '=',
       Configuration_Pragmas_File => '!',
       Mapping_File               => '!',
       Object_Path_File_Name      => '!',
       Include_Path               => ':',
       Outer_Dir                  => '=',
       Output_Directory           => '=',
       Target                     => '=',
       Subdirs                    => '=',
       Wide_Character_Encoding    => '!']);

   package Common_String_Defaults is new Common_String_Switches.Set_Defaults
     ([Project_File               => null,
       Aggregated_Project_File    => null,
       Run_Time_System            => +"", -- ????Needed?
       Configuration_Pragmas_File => null,
       Mapping_File               => null,
       Object_Path_File_Name      => null,
       Include_Path               => null,
       Outer_Dir                  => null,
       Output_Directory           => null,
       Target                     => null,
       Subdirs                    => null,
       Wide_Character_Encoding    => null]);

   package Common_String_Shorthands is new Common_String_Switches
     .Set_Shorthands
     ([Project_File               => +"-P",
       Aggregated_Project_File    => null,
       Run_Time_System            => +"--RTS",
       Configuration_Pragmas_File => +"-gnatec",
       Mapping_File               => +"-gnatem",
       Object_Path_File_Name      => +"-gnateO",
       Include_Path               => +"-I",
       Outer_Dir                  => null,
       Output_Directory           => +"--output-dir",
       Target                     => null,
       Subdirs                    => null,
       Wide_Character_Encoding    => +"-W"]);

   package Common_String_Shorthands_2 is new Common_String_Switches
     .Set_Shorthands
     ([Output_Directory => +"--dir",
       others => null]);

   --  Note: Most tools allow "--debugx" with a shorthand of "-dx".
   --  Gnatmetric, however, uses "--gnatmetric-debugx" with a shorthand
   --  of "-debugx".

   type Common_String_Seqs is (Debug, Files, Ignore, External_Variable);

   package Common_String_Seq_Switches is new String_Seq_Switches
     (Common_Descriptor,
      Common_String_Seqs);

   package Common_String_Seq_Syntax is new Common_String_Seq_Switches
     .Set_Syntax
     ([Debug => '!', Files => '=', Ignore => '=', External_Variable => '!']);

   package Common_String_Seq_Shorthands is new Common_String_Seq_Switches
     .Set_Shorthands
     ([Debug => +"-d", Files => +"-files", Ignore => null,
       External_Variable => +"-X"]);

   --  ??? -j is ignored.
   type Common_Nats is (Jobs);
   package Common_Nat_Switches is new Other_Switches
     (Common_Descriptor,
      Common_Nats,
      Natural,
      Natural'Image,
      Natural'Value);
   package Common_Nat_Syntax is new Common_Nat_Switches.Set_Syntax
     ([Jobs => '!']);
   package Common_Nat_Defaults is new Common_Nat_Switches.Set_Defaults
     ([Jobs => 1]);
   package Common_Nat_Shorthands is new Common_Nat_Switches.Set_Shorthands
     ([Jobs => +"-j"]);

   use Common_Flag_Switches, Common_String_Switches;

   Incremental_Switches : constant Switch_Array :=
     [To_All (Incremental),
      To_All (Outer_Parallel),
      To_All (Outer_Dir),
      To_All (Compile_Switch),
      To_All (No_Code_Generation),
      To_All (Avoid_Processing_Gnat_Adc),
      To_All (Mapping_File),
      To_All (Configuration_Pragmas_File),
      To_All (Object_Path_File_Name)];

   ----------------

   function Wide_Character_Encoding (Cmd : Command_Line) return String;
   function Wide_Character_Encoding
     (Cmd : Command_Line) return System.WCh_Con.WC_Encoding_Method;
   --  Libadalang wants the encoding as a String, whereas Pp.Buffers uses
   --  WC_Encoding_Method.

   procedure Set_WCEM (Cmd : in out Command_Line; Encoding : String);
   --  Set the wide character encoding method as if the switch had appeared on
   --  the command line. This is used when a BOM selects UTF-8.

   ----------------

   function Mimic_gcc (Cmd : Command_Line) return Boolean is
     (Arg (Cmd, Outer_Dir) /= null);
   --  True if this is an inner invocation of the tool for incremental mode, so
   --  that the ASIS tool should mimic the gcc compiler in certain ways.
   --
   --  Mimic_gcc is True when the tool is invoked by the builder.
   --  We use --outer-dir to detect that we were called from gprbuild.
   --
   --  When Mimic_gcc is True, the tool behavior is modified as follows:
   --
   --     - When a library unit body is processed, also process the spec and
   --       all subunits. This is necessary because the builder does not invoke
   --       the "compiler" on specs with bodies, nor on subunits. This involves
   --       setting Add_Needed_Sources ON.
   --
   --     - When the ASIS tool invokes the real compiler on a library unit body
   --       or bodiless spec, it does so in the Tool_Inner_Dir, rather than
   --       the usual Tool_Temp_Dir, so that the ALI file will be in the right
   --       place for subsequent runs of the builder.
   --
   --     - When doing cleanup, we set Keep_ALI_Files to True so the .ali files
   --       are kept around for subsequent runs of the builder.

   function Incremental_Mode_Specified (Cmd : Command_Line) return Boolean is
     (Arg (Cmd, Incremental) and then not Mimic_gcc (Cmd));
   --  We need to ignore --incremental in the inner invocation, because
   --  --incremental could be specified in package Pretty_Printer of the
   --  project file, which will cause the builder to pass it to the inner
   --  invocation.

   function Incremental_Mode_By_Default (Cmd : Command_Line) return Boolean is
     (False and then Arg (Cmd, Project_File) /= null);
   --  Change False to True to force --incremental mode ON in cases where it is
   --  legal (i.e. a project file was specified). This is for testing.

   function Incremental_Mode (Cmd : Command_Line) return Boolean is
     (Incremental_Mode_Specified (Cmd)
        or else Incremental_Mode_By_Default (Cmd));
   --  True if --incremental was given on the command line. In this mode, the
   --  ASIS tool is incremental on a file-by-file basis (e.g. don't run
   --  gnat2xml if the xml file is already up to date).
   --
   --  Incremental_Mode works like this: gnat2xml (or whatever other ASIS tool
   --  that supports this mode) invokes gprbuild, telling it to pretend that
   --  gnat2xml is the "compiler". So the builder invokes gnat2xml once for
   --  each relevant file. So we have an "outer" invocation of gnat2xml, and
   --  many "inner" invocations.
   --
   --  The command-line arguments passed to the outer gnat2xml are modified
   --  before passing them along to the builder. "--incremental" is not passed
   --  to the builder. Project-related arguments are passed to the
   --  builder. Most arguments need to be seen by the inner gnat2xmls, so they
   --  are passed to the builder after "-cargs".
   --  See Utils.Environment.Builder_Command_Line for details.

   --  In incremental mode, Incremental_Mode is True for the outer invocation,
   --  and Mimic_gcc is True for the inner invocations. In nonincremental mode,
   --  both are False. They are never both True.

end Utils.Command_Lines.Common;

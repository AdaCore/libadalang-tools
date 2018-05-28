------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . D R I V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with Ada.Directories; use Ada;
with Ada.Exceptions;
with GNAT.Byte_Order_Mark;
with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with Utils.Environment;
with Utils.Formatted_Output;
with Utils.Command_Lines.Common;   use Utils.Command_Lines.Common;
--  with Utils.Command_Lines.Common.Post;
with Utils.Projects; use Utils.Projects;
--  with Utils.Check_Parameters;
with Utils.String_Utilities; use Utils.String_Utilities;
with Utils.Tool_Names;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text;

with Libadalang;     use Libadalang;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Iterators; use Libadalang.Iterators;

package body Utils.Drivers is

   pragma Warnings (Off);
   use Common_Flag_Switches, Common_Boolean_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   use Tools;

   Name_Resolution_Failed : exception;

   procedure Resolve_Node (N : Ada_Node; Quiet : Boolean);
   --  Call P_Resolve_Names on N, which is a node for which Is_Xref_Entry_Point
   --  is True. ????No need to call P_Resolve_Names anymore, see P610-018,
   --  Tue, 21 Nov 2017 20:12:48 +0100.

   procedure Name_Resolution (Unit : Analysis_Unit);
   --  Resolve the entire unit, calling Resolve_Node on each relevant node.
   --  See libadalang_env/src/libadalang/ada/testsuite/ada/nameres.adb.

   procedure Resolve_Node (N : Ada_Node; Quiet : Boolean) is
      function Safe_Image
        (Node : Ada_Node'Class) return String is
         (if Node.Is_Null then
            "None"
          else
            Langkit_Support.Text.Image (Node.Short_Image));

      function Is_Expr (N : Ada_Node) return Boolean is
        (N.Kind in Ada_Expr);

      OK : Boolean;

      use Utils.Formatted_Output;
   begin
--      if Langkit_Support.Adalog.Debug.Debug then
--         N.Assign_Names_To_Logic_Vars;
--      end if;
      --  ???For now, catch exceptions and try to continue.
      begin
         OK := N.P_Resolve_Names;
      exception
         when Property_Error =>
            Put ("P_Resolve_Names raised Property_Error\n");
            raise Name_Resolution_Failed;
      end;

      if OK then
         for Node of Find (N, Is_Expr'Access).Consume loop
            declare
               P_Ref  : constant Basic_Decl := Node.As_Expr.P_Referenced_Decl;
               P_Type : constant Base_Type_Decl :=
                 Node.As_Expr.P_Expression_Type;
            begin
               if not Quiet then
                  Put ("Expr: \1, references \2, type is \3\n",
                       Safe_Image (Node),
                       Safe_Image (P_Ref),
                       Safe_Image (P_Type));
               end if;
            end;
         end loop;
      else
         Put ("Resolution failed for node \1\n", Safe_Image (N));
         raise Name_Resolution_Failed;
      end if;
   end Resolve_Node;

   procedure Name_Resolution (Unit : Analysis_Unit) is
      function Is_Xref_Entry_Point (N : Ada_Node) return Boolean is
        (N.P_Xref_Entry_Point);
   begin
      --  ???Name resolution does not yet work, and is turned off by default
      --  (see Syntax_Only switch; name resolution can be turned on via
      --  --no-syntax-only).  For now, catch exceptions and try to continue
      --  with the next file.
      Populate_Lexical_Env (Unit);
      for Node of Find (Root (Unit), Is_Xref_Entry_Point'Access).Consume loop
         Resolve_Node (Node, Quiet => not Debug_Flag_A);
      end loop;
   end Name_Resolution;

   procedure Post_Cmd_Line_1 (Cmd : Command_Line);
   --  This is called by Process_Command_Line after the first pass through
   --  the command-line arguments.

   function ASIS_Order_File_Names
     (X : String_Ref_Array) return String_Ref_Array;
   --  ????ASIS (or maybe just gnatmetric) seems to process files in a
   --  strange order. First, all files with bodies, in alphabetical
   --  order. Then all files without bodies, in alphabetical order.
   --  We're temporarily mimicking that here.

   procedure Post_Cmd_Line_1 (Cmd : Command_Line) is
      --  ????See lal_ul-driver.adb
      use Utils.Environment;
   begin
      for Dbg of Arg (Cmd, Command_Lines.Common.Debug) loop
         Set_Debug_Options (Dbg.all);
      end loop;

      Tool_Current_Dir := new String'(Initial_Dir);
      --  Leave Tool_Inner_Dir = null
   end Post_Cmd_Line_1;

   function ASIS_Order_File_Names
     (X : String_Ref_Array) return String_Ref_Array
   is
      use String_Ref_Sets;
      Bodies : String_Ref_Set;

      function Has_Body (S : String_Ref) return Boolean;
      function Has_Body (S : String_Ref) return Boolean is
         Body_String : aliased String := S (1 .. S'Last - 1) & "b";
      begin
         return Contains (Bodies, Body_String'Unchecked_Access);
      end Has_Body;

      Result : String_Ref_Vector;
      use String_Ref_Vectors;
   begin
      for S of X loop
         if Has_Suffix (S.all, ".adb") then
            Insert (Bodies, S);
         end if;
      end loop;

      for S of X loop
         if not Has_Suffix (S.all, ".ads") then
            Append (Result, S);
         end if;
         if Has_Suffix (S.all, ".ads") and then Has_Body (S) then
            Append (Result, S);
         end if;
      end loop;

      for S of X loop
         if Has_Suffix (S.all, ".ads") and then not Has_Body (S) then
            Append (Result, S);
         end if;
      end loop;

      return To_Array (Result);
   end ASIS_Order_File_Names;

   procedure Driver
     (Cmd                   : in out Command_Line;
      Tool                  : in out Tool_State'Class;
      Tool_Package_Name     :        String;
      Needs_Per_File_Output :        Boolean        := False;
      Preprocessing_Allowed :        Boolean        := True;
      Callback              :        Parse_Callback := null)
   is
      procedure Local_Callback
        (Phase : Parse_Phase;
         Swit  : Dynamically_Typed_Switch);
      --  This processes the Common switches, and then calls the tool-specific
      --  Callback passed in.

      procedure Process_Files;

      procedure Set_WCEM (Encoding : String);
      --  Set the wide character encoding method as if the switch had appeared
      --  on the command line (not in -cargs section). This is used when the
      --  -cargs section is used, and when a BOM selects UTF-8.

      procedure Process_Cargs;
      --  Process arguments in the -cargs sections. This is questionable, given
      --  that lalpp does not call gcc, but we support at least "-cargs -Wx"
      --  for compatibility, at least for now.

      procedure Print_Help;

      procedure Local_Callback
        (Phase : Parse_Phase;
         Swit  : Dynamically_Typed_Switch)
      is
      begin
         Callback (Phase, Swit);
      end Local_Callback;

      Cmd_Text, Cmd_Cargs, Project_Switches_Text :
        GNAT.OS_Lib.Argument_List_Access;
      Global_Report_Dir               : String_Ref;
      Compiler_Options                : GNAT.OS_Lib.Argument_List_Access;
      Custom_RTS                      : GNAT.OS_Lib.String_Access;
      Individual_Source_Options       : String_String_List_Map;
      Result_Dirs                     : String_String_Map;

      procedure Set_WCEM (Encoding : String) is
      begin
         if not Present (Arg (Cmd, Wide_Character_Encoding)) then
            Set_Arg (Cmd, Wide_Character_Encoding, Encoding);
         elsif Arg (Cmd, Wide_Character_Encoding).all /= Encoding then
            Cmd_Error_No_Help
              ("input and output wide character encodings conflict");
         end if;
      end Set_WCEM;

      procedure Process_Cargs is
      begin
         --  We actually only support -Ws, -W8, and -Wb.
         for Arg of Cmd_Cargs.all loop
            if Arg.all = "-gnatWh" then
               Set_WCEM ("h");
            elsif Arg.all = "-gnatWu" then
               Set_WCEM ("u");
            elsif Arg.all = "-gnatWs" then
               Set_WCEM ("s");
            elsif Arg.all = "-gnatWE" then
               Set_WCEM ("E");
            elsif Arg.all = "-gnatW8" then
               Set_WCEM ("8");
            elsif Arg.all = "-gnatWb" then
               Set_WCEM ("b");
            else
               null; -- Ignore all others
            end if;
         end loop;
      end Process_Cargs;

      procedure Process_Files is
         Context : Analysis_Context;
         Num_File_Names : constant Natural := File_Names (Cmd)'Length;
         Counter : Natural := Num_File_Names;
         use Text_IO;
      begin
         for F_Name of ASIS_Order_File_Names (File_Names (Cmd)) loop
            if Arg (Cmd, Verbose) then
               Put_Line
                 (Standard_Error, "[" & Image (Counter) & "] " & F_Name.all);
               --  ????Use Formatted_Output?
            elsif not Arg (Cmd, Quiet) and then Num_File_Names > 1 then
               Put
                 (Standard_Error,
                  "Units remaining: " & Image (Counter) & "     " & ASCII.CR);
            end if;
            Counter := Counter - 1;

--         Utils.Options.No_Argument_File_Specified := False;
            declare
               use GNAT.OS_Lib, GNAT.Byte_Order_Mark;
               --  We read the file into a String, and convert to wide
               --  characters according to the encoding method.
               --
               --  No matter what the encoding method is, we recognize brackets
               --  encoding, but not within comments.
               --
               --  These behaviors are intended to match what the compiler
               --  does.

               Input : String_Access := Read_File (F_Name.all);
               First : Natural       := 1;

               BOM     : BOM_Kind;
               BOM_Len : Natural;
               BOM_Seen : Boolean := False;
            begin
               --  Check for BOM at start of file. The only supported BOM is
               --  UTF8_All. If present, when we're called from gnatpp, the
               --  Wide_Character_Encoding should already be set to
               --  WCEM_UTF8, but when we're called from xml2gnat, we need to
               --  set it.

               Read_BOM (Input.all, BOM_Len, BOM);
               if BOM = UTF8_All then
                  First := BOM_Len + 1; -- skip it
                  BOM_Seen := True;
                  Set_WCEM ("8");
               else
                  pragma Assert (BOM = Unknown); -- no BOM found
               end if;

               --  We have to defer the Create call until after we've read the
               --  first file, because it might set the Wide_Character_Encoding
               --  via the BOM. This makes the somewhat questionable assumption
               --  that all files have the same encoding (which is necessary
               --  anyway if it's controlled by the command line).

               if Counter = Num_File_Names - 1 then
                  Context := Create
                    (Charset => Wide_Character_Encoding (Cmd),
                     With_Trivia => True);
               end if;

               declare
                  Inp : String renames Input (First .. Input'Last);
                  Unit : constant Analysis_Unit := Get_From_Buffer
                    (Context, F_Name.all,
                     Buffer => Inp);
               begin
                  if Has_Diagnostics (Unit) then
                     Put_Line ("Errors while parsing " & F_Name.all);
                     for D of Diagnostics (Unit) loop
                        Put_Line
                          (Langkit_Support.Diagnostics.To_Pretty_String (D));
                        --  To stderr????
                     end loop;

                     if Root (Unit).Is_Null then
                        goto Continue;
                     end if;
                  end if;

                  --  We continue even in the presence of errors (if we have a
                  --  tree).

                  pragma Assert (not Root (Unit).Is_Null);
                  if False and then
                    not Arg (Cmd, Syntax_Only)
                  then
                     Name_Resolution (Unit);
                  end if;
                  Per_File_Action (Tool, Cmd, F_Name.all, Inp, BOM_Seen, Unit);
                  --  ???For now, catch exceptions and try to continue.
                  begin
                     Remove (Context, F_Name.all);
                     --  ???This Remove might be an efficiency issue.
                     --  We could try removing Remove here, but then
                     --  it might eat up too much memory.
                     --  The libadalang project is considering using
                     --  a cache to solve both problems.
                  exception
                     when E : Constraint_Error =>
                        Put_Line ("Remove raised Constraint_Error");
                        Put_Line ("Traceback:");
                        Put (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
                        Put_Line ("End traceback");
                        raise Name_Resolution_Failed;
                  end;
                  Free (Input);
               end;
            exception
               when Name_Resolution_Failed =>
                  Free (Input);
            end;

            <<Continue>>
         end loop;
         Destroy (Context);
         pragma Assert (Counter = 0);
      end Process_Files;

      procedure Print_Help is
      begin
         Tool_Help (Tool);
      end Print_Help;

   --  Start of processing for Driver

   begin
      Environment.Create_Temp_Dir;

      Process_Command_Line
        (Cmd,
         Cmd_Text,
         Cmd_Cargs,
         Project_Switches_Text,
         Global_Report_Dir,
         Compiler_Options,
         Project_RTS               => Custom_RTS,
         Individual_Source_Options => Individual_Source_Options,
         Result_Dirs               => Result_Dirs,
         The_Project_Tree          => Tool.Project_Tree,
         Needs_Per_File_Output     => Needs_Per_File_Output,
         Preprocessing_Allowed     => Preprocessing_Allowed,
         Tool_Package_Name         => Tool_Package_Name,
         Callback                  => Local_Callback'Unrestricted_Access,
         Post_Cmd_Line_1_Action    => Post_Cmd_Line_1'Access,
         Tool_Temp_Dir             => Environment.Tool_Temp_Dir.all,
         Print_Help                => Print_Help'Access);
--      Utils.Command_Lines.Common.Post.Postprocess_Common (Cmd);
      Process_Cargs;

      if Debug_Flag_C then
         Dump_Cmd (Cmd);
      end if;

--      Utils.Check_Parameters; -- ????Move into Init?

      --  ????????????????Stuff from Environment:

      declare
         use GNAT.OS_Lib, Environment;
      begin
         Copy_Gnat_Adc;
         pragma Assert
           (Get_Current_Dir = Tool_Current_Dir.all & Directory_Separator);

--         if not Incremental_Mode then
--            Change_Dir (Tool_Temp_Dir.all);
--            Utils.Compiler_Options.Store_I_Options;
--         end if;

         --  Create output directory if necessary

--         if Out_Dir /= null then
--            Parallel_Make_Dir (Out_Dir.all, Give_Message => Verbose_Mode);
--         end if;
      end;

      --  In Incremental_Mode, we invoke the builder instead of doing the
      --  normal tool processing. The inner invocations of this tool invoked by
      --  the builder will do the normal tool processing.

--      if Utils.Options.Incremental_Mode then
--         Environment.Call_Builder;
--      else
--         Utils.Source_Table.Processing.Process_Sources;
--      end if;

      --  Create output directory if necessary

      if Present (Arg (Cmd, Output_Directory)) then
         declare
            Dir : constant String := Arg (Cmd, Output_Directory).all;
            Cannot_Create : constant String :=
              "cannot create directory '" & Dir & "'";
            use Directories;
         begin
            if Exists (Dir) then
               if Kind (Dir) /= Directory then
                  Cmd_Error (Cannot_Create & "; file already exists");
               end if;
            else
               begin
                  Create_Directory (Dir);
               exception
                  when Name_Error | Use_Error =>
                     Cmd_Error (Cannot_Create);
               end;
            end if;
         end;
      end if;

      Init (Tool, Cmd);
      Process_Files;
      Final (Tool, Cmd);

      Environment.Clean_Up;

--      if not Utils.Options.Incremental_Mode then
--         if not Utils.Source_Table.Processing
--             .All_Files_Successfully_Processed
--         then
--            GNAT.OS_Lib.OS_Exit (1);
--         end if;
--      end if;

      Utils.Main_Done := True;

   exception
      when X : File_Not_Found =>
         declare
            use Text_IO, Ada.Exceptions, Utils.Tool_Names;
         begin
            Put_Line
              (Standard_Error, Tool_Name & ": " & Exception_Message (X));
         end;
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      when Utils.Command_Lines.Command_Line_Error =>
         --  Error message has already been printed.
         GNAT.Command_Line.Try_Help;
         Environment.Clean_Up;
--         GNAT.OS_Lib.OS_Exit (1);
      when Utils.Command_Lines.Command_Line_Error_No_Help |
        Utils.Command_Lines.Command_Line_Error_No_Tool_Name =>
         --  Error message has already been printed.
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
   end Driver;

end Utils.Drivers;

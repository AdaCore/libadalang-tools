with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding;
with System.WCh_Con;
with System.WCh_Cnv;
with Stub.Command_Lines; use Stub.Command_Lines;

with Ada.Directories; use Ada.Directories;
with Interfaces; use type Interfaces.Unsigned_16;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Langkit_Support.Slocs; use Langkit_Support;
with Langkit_Support.Diagnostics;
with Libadalang;     use Libadalang;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL.Common;
with ASIS_UL.Dbg_Out;
with LAL_UL.Formatted_Output;
with LAL_UL.Tool_Names;
with ASIS_UL.Char_Vectors; use ASIS_UL.Char_Vectors;
use ASIS_UL.Char_Vectors.Char_Vectors;
with GNATCOLL.VFS;
with GNATCOLL.Projects;
with ASIS_UL.Generic_Formatted_Output;

with ASIS_UL.Debug; use ASIS_UL.Debug;

with LAL_UL.Environment;

with Pp.Actions;
with Pp.Command_Lines;

package body Stub.Actions is

   function Image (X : Integer) return String
     renames ASIS_UL.String_Utilities.Image;

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Stub_Flag_Switches,
     Stub_String_Switches,
     Stub_Nat_Switches;

   ----------
   -- Init --
   ----------

   procedure Init (Tool : in out Stub_Tool; Cmd : Command_Line) is
      pragma Unreferenced (Tool, Cmd);
   begin
      --  ????Other checks from gnatstub/lal_ul-check_parameters.adb?

      pragma Assert (Environment.Initial_Dir = Current_Directory);
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Stub_Tool; Cmd : Command_Line) is
   begin
      null;
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   --  Debugging printouts:
   --  See also Libadalang.Debug.
   pragma Warnings (Off);
   pragma Style_Checks (Off);
   procedure knd (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure psloc (X : Ada_Node) is

      function Lines_String
        (Sloc_Range : Slocs.Source_Location_Range) return String is
         (Image (Integer (Sloc_Range.Start_Line)) & ": " &
          Image (Integer (Sloc_Range.End_Line)));

      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", Lines_String (Sloc_Range (X)));
   end psloc;

   procedure nn (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X = null then "null" else Short_Image (X)));
   end nn;

   procedure ppp (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      nn (X);
      Print (X);
   end ppp;

   procedure Put_Ada_Node_Array (X : Ada_Node_Array) is
      use ASIS_UL.Dbg_Out;
   begin
      for N of X loop
         nn (N);
         Put ("----------------\n");
      end loop;
   end Put_Ada_Node_Array;

   procedure Put_Child_Record (C : Child_Record) is
      use ASIS_UL.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", Short_Image (C.Node));
         when Trivia =>
            declare
               Trivia_Data : constant Token_Data_Type := Data (C.Trivia);
            begin
               Put ("Trivia: \1 ""\2"" \3\n",
                    Kind (Trivia_Data)'Img,
                    To_UTF8 (Text_To_W_Str (Text (C.Trivia))),
                    Slocs.Image (Sloc_Range (Trivia_Data)));
            end;
      end case;
   end Put_Child_Record;

   procedure Put_Children_Array (A : Children_Array) is
      use ASIS_UL.Dbg_Out;
   begin
      for I in A'Range loop
         Put ("\1: ", Image (I));
         Put_Child_Record (A (I));
      end loop;
   end Put_Children_Array;

   procedure Dump
     (Tool : in out Stub_Tool;
      Message : String := "")
   is
      pragma Unreferenced (Tool);
      use LAL_UL.Formatted_Output;
   begin
      if Debug_Flag_V then
         Put ("\1\n", Message);
      end if;
   end Dump;
   pragma Style_Checks (On);
   pragma Warnings (On);

   procedure Generate
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Root_Node : Ada_Node;
      Parent_Body_Of_Subunit : Ada_Node);
   --  Given a spec, generate the body file. Given a body, recursively call
   --  Generate on any Ada stubs, and given a stub, generate the subunit file.
   --
   --  Note: "stub" is used in two different ways. In Ada, a stub ends with "is
   --  separate;", and we generate subunits for those. But the "stubs"
   --  generated by gnatstub are not stubs in the Ada sense; they are proper
   --  bodies.

   function Q (S : W_Str) return W_Str is
     (if S (S'First) = '"' then """" & S & """" else S);
   --  S is the name of a program unit. If it's an operator symbol, like
   --  "and", we return ""and"" so it can be used inside a generated string
   --  literal. If it's an identifier, we return it unchanged.

   function Intersperse_Spaces (S : W_Str) return W_Str;
   --  Put a space between each character, so "Foo" --> "F o o".
   --  Used in generating header comments.

   function Centered_Comment (S : W_Str; Len : Natural) return W_Str;
   --  Return something like "--    S    --" of the right length.
   --  Used in generating header comments.

   function Needs_Completion (N : Ada_Node) return Boolean;
   --  Returns True if N is a declaration that needs a completion.

   function Get_Parent_Name (Parent_Body_Of_Subunit : Ada_Node) return W_Str;
   --  This is the name to put in "separate (...)" when generating a subunit.

   function Less_Than (X, Y : Ada_Node) return Boolean;
   package Sorting is new Ada_Node_Vectors.Generic_Sorting (Less_Than);
   --  Used to implement the Alphabetical_Order switch (-gnatyo).
   --  Type declarations come first, in alphabetical order, then bodies, in
   --  alphabetical order. If two or more subprograms have the same name, they
   --  retain their order in the spec. Here we are sorting the incomplete types
   --  and specs into the order their completions should appear in.

   function Get_Pp_Cmd return Command_Line;
   --  Return a command line for passing to the pretty printer.

   Comments_Fill_Arg : aliased String := "--comments-fill";
   Decimal_Grouping : aliased String := "--decimal-grouping=3";
   Based_Grouping : aliased String := "--based-grouping=4";
   Args : aliased GNAT.OS_Lib.Argument_List :=
     (Comments_Fill_Arg'Unchecked_Access,
      Decimal_Grouping'Unchecked_Access,
      Based_Grouping'Unchecked_Access);

   function Get_Pp_Cmd return Command_Line is
   begin
      return Result : Command_Line (Pp.Command_Lines.Descriptor'Access) do
         Parse (Args'Access, Result,
                Cmd_Line_1, Null_Callback'Access,
                Collect_File_Names => False);
      end return;
   end Get_Pp_Cmd;

   function Intersperse_Spaces (S : W_Str) return W_Str is
      use WChar_Vectors;
      Result : WChar_Vector;
      First_Time : Boolean := True;
   begin
      for C of S loop
         if First_Time then
            First_Time := False;
         else
            Append (Result, ' ');
         end if;

         Append (Result, C);
      end loop;

      return To_Array (Result);
   end Intersperse_Spaces;

   function Centered_Comment (S : W_Str; Len : Natural) return W_Str is
      Num_Spaces : constant Integer := Len - S'Length - 4;
   begin
      if Num_Spaces < 0 then
         return "--" & S & "--";
      else
         declare
            L : constant Natural := Num_Spaces / 2;
            R : constant Natural :=
              (if Num_Spaces mod 2 = 0 then L else L + 1);
         begin
            return "--" & (1 .. L => ' ') & S & (1 .. R => ' ') & "--";
         end;
      end if;
   end Centered_Comment;

   function Less_Than (X, Y : Ada_Node) return Boolean is
      X_Name : constant W_Str := L_Name (Get_Def_Name (X));
      Y_Name : constant W_Str := L_Name (Get_Def_Name (Y));
      use Slocs;
      X_Sloc : constant Source_Location := Start_Sloc (Sloc_Range (X));
      Y_Sloc : constant Source_Location := Start_Sloc (Sloc_Range (Y));
   begin
      if X.Kind in Ada_Type_Decl and then Y.Kind not in Ada_Type_Decl then
         return True;
      end if;

      if X.Kind not in Ada_Type_Decl and then Y.Kind in Ada_Type_Decl then
         return False;
      end if;

      if X_Name = Y_Name then
         return Compare (X_Sloc, Y_Sloc) = After; -- Make the sort stable.
      end if;

      return X_Name < Y_Name;
   end Less_Than;

   function Needs_Completion (N : Ada_Node) return Boolean is
   begin
      --  ???This is incomplete. It should return True for an incomplete type
      --  in a private part, but only if the incomplete type is not completed
      --  in the private part. For a package, it should return True only if
      --  there is something in it that needs completion.

      case N.Kind is
         when Ada_Package_Decl | Ada_Generic_Package_Decl => return True;
         when Ada_Single_Protected_Decl | Ada_Protected_Type_Decl |
           Ada_Single_Task_Decl | Ada_Task_Type_Decl => return True;
         when Ada_Entry_Decl => return True;
         when Ada_Subp_Decl | Ada_Generic_Subp_Decl => return True;
         when others => return False;
      end case;
   end Needs_Completion;

   function Get_Parent_Name (Parent_Body_Of_Subunit : Ada_Node) return W_Str is
   begin
      if Parent_Body_Of_Subunit = null then
         return "";
      elsif Parent (Parent_Body_Of_Subunit).Kind in Ada_Subunit then
         declare
            Parent_Parent : constant W_Str :=
              Full_Name (F_Name (Subunit (Parent (Parent_Body_Of_Subunit))));
            Parent_Simple : constant W_Str :=
              Id_Name (Get_Def_Name (Parent_Body_Of_Subunit));
         begin
            return Parent_Parent & "." & Parent_Simple;
         end;
      else
         return Full_Name (Get_Def_Name (Parent_Body_Of_Subunit));
      end if;
   end Get_Parent_Name;

   procedure Generate
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Root_Node : Ada_Node;
      Parent_Body_Of_Subunit : Ada_Node)
   is
      Looking_For_Ada_Stubs : constant Boolean :=
        Root_Node.Kind in Ada_Body_Node
        and then Root_Node.Kind not in Ada_Body_Stub;
      Parent_Name : constant W_Str := Get_Parent_Name (Parent_Body_Of_Subunit);
      Root_Node_Name : constant W_Str :=
        (if Parent_Body_Of_Subunit /= null then Parent_Name & "." else "") &
        Full_Name (Get_Def_Name (Root_Node));
      UC_Root_Node_Name : constant W_Str := To_Upper (Root_Node_Name);
      LC_Root_Node_Name : constant W_Str := To_Lower (Root_Node_Name);

      Wide_Char_Encoding : constant System.WCh_Con.WC_Encoding_Method :=
        Wide_Character_Encoding (Cmd);
      Out_Vec, Pp_Out_Vec : Char_Vector;

      procedure Put_To_Out_Vec (WC : W_Char);
      procedure Put_To_Out_Vec (WC : W_Char) is
         procedure Append_One (C : Character);
         pragma Inline (Append_One);
         procedure Append_One (C : Character) is
         begin
            Append (Out_Vec, C);
         end Append_One;
         procedure Encode is new
           System.WCh_Cnv.Wide_Char_To_Char_Sequence (Append_One);
      begin
         Encode (WC, Wide_Char_Encoding);
      end Put_To_Out_Vec;

      package Buffered_Output is new ASIS_UL.Generic_Formatted_Output
        (W_Char,
         W_Str,
         Basic_Put_Char => Put_To_Out_Vec);
      use Buffered_Output;

      procedure Generate_CU_Header;

      procedure Walk (Decl : Ada_Node; Level : Natural);
      --  Generate code corresponding to Decl, and recursively walk subtrees.
      --
      --  Note on recursion: Generate calls Walk.  Walk calls Walk.  Walk calls
      --  generate. In more detail: Generate is called on the compilation unit
      --  spec or body. In the case of a spec, Generate calls Walk on the spec,
      --  which calls Walk on nested specs, and generates a body for each one.
      --  In the case of a body, Generate calls Walk on the body, which calls
      --  Generate on nested Ada body stubs, to generate subunit files for each
      --  one.

      procedure Generate_Local_Header (Name : W_Str; Level : Natural);
      --  Generate the local header that appears before each body,
      --  unless the --no_local_header switch was given.

      procedure Generate_Subunit_Start;
      --  If we are processing a subunit, generate "separate (parent)".

      procedure Generate_Stub_Begin_End (Name, Stub_Kind : W_Str);
      --  Generate the text from "begin" to "end" of the generated code for a
      --  subprogram, entry, or task body. Stub_Kind is "function",
      --  "procedure", "entry", or "task".

      procedure Format;
      --  Call the pretty printer on Out_Vec, producing Pp_Out_Vec.

      procedure Write_Output_File;
      --  Write the content of Pp_Out_Vec to the output file

      function Get_Output_Name (Resolve_Links : Boolean) return String;
      --  Return the name of the output file

      procedure Generate_CU_Header is
      begin
         if Arg (Cmd, Header_File) /= null then
            declare
               Header : String_Access :=
                 Read_File (Arg (Cmd, Header_File).all);
            begin
               Put ("\1", From_UTF8 -- ????wrong
                      (Header.all));
               Free (Header);
            end;
         end if;

         if Arg (Cmd, Comment_Header_Sample) then
            declare
               With_Spaces : constant W_Str :=
                 Intersperse_Spaces (UC_Root_Node_Name);
               Len : constant Natural := Arg (Cmd, Max_Line_Length);
               Dashes : constant W_Str := (1 .. Len => '-');
               Spaces : constant W_Str := "--" & (1 .. Len - 4 => ' ') & "--";
            begin
               Put ("\1\n", Dashes);
               Put ("\1\n", Spaces);
               Put ("\1\n", Centered_Comment (With_Spaces, Len));
               Put ("\1\n", Spaces);
               Put ("\1\n", Centered_Comment ("B o d y", Len));
               Put ("\1\n", Spaces);
               Put ("\1\n", Dashes);
               Put ("\n");
            end;
         end if;

         if Arg (Cmd, Comment_Header_Spec) then
            declare
               pragma Assert (Input'First = 1);
               Last : Natural := 0;
               Next : Positive := 1;
            begin
               --  We want to copy comment lines from the input, starting at
               --  the start of the file, and stopping when we get to a
               --  noncomment line.  Se set Last to point to the NL at the
               --  end of the last comment line in the header.

               while Next <= Input'Last - 2 loop
                  while Next <= Input'Last - 2 and then
                    Input (Next) in ' ' | ASCII.HT
                  loop
                     Next := Next + 1;
                  end loop;
                  exit when Input (Next .. Next + 1) /= "--";
                  while Next <= Input'Last and Input (Next) /= ASCII.LF loop
                     Next := Next + 1;
                  end loop;
                  Last := Next;
                  Next := Next + 1;
               end loop;

               --  Now the slice ending at Last is what we want to copy,
               --  replacing the "S p e c" string, if any.

               Append (Out_Vec,
                       Replace_String (Input (1 .. Last),
                                       From => "S p e c", To => "B o d y"));
               Put ("\n");
            end;
         end if;
      end Generate_CU_Header;

      procedure Generate_Local_Header (Name : W_Str; Level : Natural) is
      begin
         if not Arg (Cmd, No_Local_Header) and then Level > 0 then
            declare
               Header_Length : constant Natural := Name'Length + 6;
               Header_Line : constant W_Str := (1 .. Header_Length => '-');
            begin
               Put (" \1\n", Header_Line);
               Put (" -- \1 --\n", Name);
               Put (" \1\n\n", Header_Line);
            end;
         end if;
      end Generate_Local_Header;

      procedure Generate_Subunit_Start is
      begin
         if Parent_Body_Of_Subunit /= null then
            Put ("separate (\1)\n", Parent_Name);
         end if;
      end Generate_Subunit_Start;

      procedure Generate_Stub_Begin_End (Name, Stub_Kind : W_Str) is
         Returns : constant Boolean := Stub_Kind = "function";
      begin
         Put ("begin\n");
         Put (" --  Generated stub: replace with real body!\n");
         Put ("pragma Compile_Time_Warning " &
                "(Standard.True, ""\1 unimplemented"");\n",
              Q (Name));

         if Arg (Cmd, No_Exception) and then not Returns then
            --  ????This is wrong; should usually generate
            --  "return Result;" if No_Exception.
            Put ("null;");
         else
            Put ("\1raise Program_Error with ""Unimplemented \2 \3"";\n",
                 (if Returns then "return " else ""), Stub_Kind, Q (Name));
         end if;

         Put ("end;\n");
      end Generate_Stub_Begin_End;

      procedure Walk (Decl : Ada_Node; Level : Natural) is
         Local_Decls : Ada_Node_Vector;
         Name : constant W_Str := Full_Name (Get_Def_Name (Decl));
         use Ada_Node_Vectors;

         procedure Collect_Local_Decls (Decls : Ada_Node_List);
         --  Append all the declarations that need recursive processing onto
         --  Local_Decls.

         procedure Collect_Local_Decls (Decls : Ada_Node_List) is
         begin
            for X in 1 .. Last_Child_Index (Decls) loop
               declare
                  Subtree : constant Ada_Node := Childx (Decls, X);
               begin
                  if (Looking_For_Ada_Stubs
                       and then Subtree.Kind in Ada_Body_Stub)
                    or else
                     (not Looking_For_Ada_Stubs
                       and then Needs_Completion (Subtree))
                  then
                     Append (Local_Decls, Subtree);
                  end if;
               end;
            end loop;
         end Collect_Local_Decls;

      --  Start of processing for Walk

      begin
         if Level > 0 then
            Put ("\n");
         end if;

         --  For things we will recursively walk (packages, protecteds),
         --  generate the start of the body. For other things (tasks,
         --  subprograms, entries), generate the body.

         case Decl.Kind is
            when Ada_Package_Decl | Ada_Generic_Package_Decl |
              Ada_Package_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start;
               Put ("package body \1 is\n", Name);
            when Ada_Single_Protected_Decl | Ada_Protected_Type_Decl |
              Ada_Protected_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start;
               Put ("protected body \1 is\n", Name);
            when Ada_Single_Task_Decl | Ada_Task_Type_Decl |
              Ada_Task_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start;
               Put ("task body \1 is\n", Name);
               Generate_Stub_Begin_End (Name, "task");

            when Ada_Subp_Decl | Ada_Generic_Subp_Decl | Ada_Subp_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start;
               declare
                  Pp_Tool : Pp.Actions.Pp_Tool;
                  Pp_Cmd : constant Command_Line := Get_Pp_Cmd;
                  Empty_Vec, Pp_Out_Vec : Char_Vector;
                  Spec : constant Subp_Spec := Get_Subp_Spec (Decl);
                  Overrides : constant Ada_Overriding_Node :=
                    (if Decl.Kind in Ada_Classic_Subp_Decl
                       then F_Overriding (Classic_Subp_Decl (Decl))
                       else Ada_Overriding_Unspecified);
                  Returns : constant Boolean := F_Subp_Returns (Spec) /= null;
               begin
                  Pp.Actions.Format_Vector
                    (Pp_Tool, Pp_Cmd,
                     File_Name => "",
                     Input => Empty_Vec,
                     Output => Pp_Out_Vec,
                     Node => Ada_Node (Spec));
                  Put ("\1 \2 is\n",
                       (case Overrides is
                          when Ada_Overriding_Not_Overriding =>
                            "not overriding ",
                          when Ada_Overriding_Overriding =>
                            "overriding ",
                          when Ada_Overriding_Unspecified => ""),
                       From_UTF8 -- ????wrong
                         (Elems (Pp_Out_Vec) (1 .. Last_Index (Pp_Out_Vec))));
                  Generate_Stub_Begin_End
                    (Name, (if Returns then "function" else "procedure"));
               end;

            when Ada_Entry_Decl =>
               Generate_Local_Header (Name, Level);
               declare
                  Pp_Tool : Pp.Actions.Pp_Tool;
                  Pp_Cmd : constant Command_Line := Get_Pp_Cmd;
                  Empty_Vec, Pp_Out_Vec : Char_Vector;
                  Parms : constant Params :=
                    F_Params (Entry_Decl (Decl));
                  Overrides : constant Ada_Overriding_Node :=
                     F_Overriding (Entry_Decl (Decl));
               begin
                  if Parms /= null then
                     Pp.Actions.Format_Vector
                       (Pp_Tool, Pp_Cmd,
                        File_Name => "",
                        Input => Empty_Vec,
                        Output => Pp_Out_Vec,
                        Node => Ada_Node (Parms));
                     Put ("\1entry \2(\3) when Standard.True is\n",
                          (case Overrides is
                             when Ada_Overriding_Not_Overriding =>
                               "not overriding ",
                             when Ada_Overriding_Overriding =>
                               "overriding ",
                             when Ada_Overriding_Unspecified => ""),
                          Name,
                          From_UTF8 -- ????wrong
                            (Elems (Pp_Out_Vec)
                               (1 .. Last_Index (Pp_Out_Vec))));
                  else
                     Put ("\1entry \2 when Standard.True is\n",
                          (case Overrides is
                             when Ada_Overriding_Not_Overriding =>
                               "not overriding ",
                             when Ada_Overriding_Overriding =>
                               "overriding ",
                             when Ada_Overriding_Unspecified => ""),
                          Name);
                  end if;

                  Generate_Stub_Begin_End (Name, "entry");
               end;

            when Ada_Subp_Body | Ada_Package_Body | Ada_Task_Body |
              Ada_Protected_Body => null;
            when others => raise Program_Error;
         end case;

         --  Collect in Local_Decls all the nested declarations or body stubs
         --  that need recursive processing. We don't process them right away,
         --  because we might need to sort the list.

         case Decl.Kind is
            when Ada_Package_Decl | Ada_Generic_Package_Decl |
              Ada_Single_Protected_Decl | Ada_Protected_Type_Decl =>
               if Vis_Part (Decl) /= null then
                  Collect_Local_Decls (F_Decls (Vis_Part (Decl)));
               end if;

               if Priv_Part (Decl) /= null then
                  Collect_Local_Decls (F_Decls (Priv_Part (Decl)));
               end if;
            when Ada_Subp_Body | Ada_Package_Body | Ada_Task_Body |
              Ada_Protected_Body =>
               Collect_Local_Decls (F_Decls (Body_Decls (Decl)));

            when Ada_Package_Body_Stub | Ada_Protected_Body_Stub =>
               null; -- ????We should find the corresponding spec,
               --  and walk the decls therein.

            when Ada_Subp_Decl | Ada_Generic_Subp_Decl |
              Ada_Single_Task_Decl | Ada_Task_Type_Decl |
              Ada_Entry_Decl |
              Ada_Task_Body_Stub | Ada_Subp_Body_Stub => null;

            when others => raise Program_Error;
         end case;

         --  Sort the list if appropriate. There's no point in sorting if we're
         --  doing subunits, because each one goes in a separate file.

         if Arg (Cmd, Alphabetical_Order) and not Looking_For_Ada_Stubs then
            Sorting.Sort (Local_Decls);
         end if;

         --  Recursively process the nested declarations. In the case of Ada
         --  stubs, we call Generate, because the corrsponding subunit goes in
         --  a separate file, and Generate knows how to create files.

         for Child of Local_Decls loop
            if Looking_For_Ada_Stubs then
               pragma Assert (Child.Kind in Ada_Body_Stub);
               Generate (Tool, Cmd, File_Name, Input, BOM_Seen,
                         Root_Node => Child,
                         Parent_Body_Of_Subunit => Root_Node);
            else
               Walk (Child, Level + 1);
            end if;
         end loop;

         case Decl.Kind is
            when Ada_Package_Decl | Ada_Generic_Package_Decl |
              Ada_Package_Body_Stub |
              Ada_Single_Protected_Decl | Ada_Protected_Type_Decl |
              Ada_Protected_Body_Stub =>
               Put ("\nend;\n");
            when Ada_Subp_Decl | Ada_Generic_Subp_Decl |
              Ada_Subp_Body_Stub |
              Ada_Single_Task_Decl | Ada_Task_Type_Decl |
              Ada_Task_Body_Stub |
              Ada_Entry_Decl |
              Ada_Subp_Body | Ada_Package_Body | Ada_Task_Body |
              Ada_Protected_Body => null;
            when others => raise Program_Error;
         end case;
      end Walk;

      procedure Format is
         Pp_Tool : Pp.Actions.Pp_Tool;
         Pp_Cmd : constant Command_Line := Get_Pp_Cmd;

         Context : Analysis_Context :=
           Create (Charset => Wide_Character_Encoding (Cmd));
         Out_Str : String renames Elems (Out_Vec) (1 .. Last_Index (Out_Vec));
         Out_Unit : constant Analysis_Unit := Get_From_Buffer
           (Context, Filename => "????",
            Buffer => Out_Str,
            With_Trivia => True);
      begin
         if False then -- ????
            pragma Assert (not Has_Diagnostics (Out_Unit));
            pragma Assert (Root (Out_Unit) /= null);
         elsif Has_Diagnostics (Out_Unit) then
            Formatted_Output.Put ("errors while parsing generated code\n");
            for D of Analysis.Diagnostics (Out_Unit) loop
               Formatted_Output.Put
                 ("\1\n", Langkit_Support.Diagnostics.To_Pretty_String (D));
            end loop;
            Pp_Out_Vec := Out_Vec;
         else
            Pp.Actions.Format_Vector
              (Pp_Tool, Pp_Cmd,
               File_Name => "",
               Input => Out_Vec,
               Output => Pp_Out_Vec,
               Node => Root (Out_Unit));
         end if;

         Remove (Context, File_Name => "????");
         Destroy (Context);
      end Format;

      function Get_Output_Name (Resolve_Links : Boolean) return String is
         pragma Unreferenced (Resolve_Links);

         use GNATCOLL.Projects, GNATCOLL.VFS;

         function Default_Name return String;
         --  This is used when the output file is not specified on the
         --  command line, and there is no project file. It uses the
         --  default GNAT converntions for file names (.ads, .adb).

         function Name_From_Project return String;
         --  This is used when there is a project file. It queries the
         --  project, so it uses whatever naming convention is specified in
         --  the project.

         function Default_Name return String is
         begin
            if Has_Suffix (File_Name, Suffix => ".ads")
              or else Has_Suffix (File_Name, Suffix => ".adb")
            then
               if Root_Node.Kind in Ada_Body_Stub then
                  declare
                     Root_Node_Name_String : constant String :=
                       To_UTF8 (LC_Root_Node_Name);
                     --  Root_Node is the stub (in the Ada sense).
                  begin
                     return Replace_String
                       (Root_Node_Name_String, From => ".", To => "-") &
                       ".adb";
                  end;
               else
                  return Result : String := File_Name do
                     Result (Result'Last) := 'b';
                  end return;
               end if;
            else
               Cmd_Error
                 ("output file name should be provided because " &
                    File_Name &
                    " does not follow GNAT naming rules for " &
                    "spec files");
            end if;
         end Default_Name;

         function Name_From_Project return String is
            Arg_Virt_File : constant Virtual_File :=
              Create (Tool.Project_Tree.all, +File_Name);
            Arg_File_Info : constant File_Info :=
              Info (Tool.Project_Tree.all, Arg_Virt_File);
         begin
            --  ????Doesn't put it in the right directory.
            --  Doesn't work for subunits.
            return +GNATCOLL.Projects.File_From_Unit
                (Project         => Project (Arg_File_Info),
                 Unit_Name       => Unit_Name (Arg_File_Info),
                 Part            => Unit_Body,
                 Language        => "Ada",
                 File_Must_Exist => False);
         end Name_From_Project;

         --  If the output file is specified on the command line,
         --  use that. Otherwise, if there is a project file,
         --  use that. Otherwise use the default naming convention.

         Simple : constant String := -- ???
           (if Arg (Cmd, Output) = null then
              (if Status (Tool.Project_Tree.all) = Empty
                 then Default_Name
                 else Name_From_Project)
            else Arg (Cmd, Output).all);
      begin
         return Result : constant String :=
           (if Arg (Cmd, Output_Directory) = null
              then Simple
              else Compose (Arg (Cmd, Output_Directory).all,
                            Simple_Name (Simple)))
         do
            if Debug_Flag_C then
               Formatted_Output.Put ("writing \1?\n", Result);
            end if;
         end return;
      end Get_Output_Name;

      Output_Name : constant String :=
        Get_Output_Name (Resolve_Links => True);

      procedure Write_Output_File is
         Out_File : constant File_Descriptor :=
           Create_File (Output_Name, Fmode => Binary);
         Out_String : String renames
           Elems (Pp_Out_Vec) (1 .. Last_Index (Pp_Out_Vec));
         Status : Boolean;

      --  Start of Processing for Write_Output_File

      begin
         if Out_File = Invalid_FD then
            raise Program_Error with
              "write of " & Output_Name & " failed";
         end if;

         if BOM_Seen then
   --         if Options.Output_Encoding /= System.WCh_Con.WCEM_UTF8 then
   --            raise Program_Error;
   --         end if;
            Write_File (Out_File, Ada.Strings.UTF_Encoding.BOM_8);
--            Put (W_Char'Val (16#FEFF#)); -- BOM as a wide character
         end if;

         Write_File (Out_File, Out_String);

         Close (Out_File, Status);
         if not Status then
            raise Program_Error with
              "write of " & Output_Name & " failed";
         end if;

         if Debug_Flag_C then
            Formatted_Output.Put ("wrote \1\n", Output_Name);
         end if;

         if not Arg (Cmd, Quiet) then
            if Root_Node.Kind in Ada_Body_Stub then
               Formatted_Output.Put
                 ("separate body is created for stub for \1\n",
                  To_UTF8 (Id_Name (Get_Def_Name (Root_Node))));
            else
               Formatted_Output.Put ("body is created for \1\n", File_Name);
            end if;
         end if;
      end Write_Output_File;

   --  Start of processing for Generate

   begin
      --  If we are looking for Ada stubs, then of course the body file exists;
      --  we are not going to overwrite it. If we are generating a subunit for
      --  an Ada stub, and the subunit file already exists, we simply skip
      --  it. If we are generating a body for an Ada spec, it is an error if
      --  the output already exists, unless --force was given.

      if not Looking_For_Ada_Stubs and then Exists (Output_Name) then
         if Root_Node.Kind in Ada_Body_Stub then
            goto Skip;
         else
            if not Arg (Cmd, Force) then
               Cmd_Error
                 ("the body for " & File_Name & " already exists; " &
                    "use -f to overwrite it");
            end if;
         end if;
      end if;

      Generate_CU_Header;
      Put ("pragma Ada_2012;\n");
      Walk (Root_Node, Level => 0);

      --  If we're processing a body for stubs, we don't want to output
      --  anything corresponding to that body; we've already output the
      --  subunits. In that case, the above Walk will have generated some
      --  rubbage, which we ignore. Looking_For_Ada_Stubs will be False if the
      --  input is a spec, so we're generating bodies, or if we're in a
      --  recursive call to Generate for an Ada stub. It will be True only for
      --  the outer call with a body as input.

      if not Looking_For_Ada_Stubs then
         Format;
         Write_Output_File;
      end if;

      <<Skip>>
   end Generate;

   procedure Per_File_Action
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
   is
      Lib_Item_Or_Subunit : constant Ada_Node :=
        F_Body (Compilation_Unit (Root (Unit)));
      Root_Node : constant Ada_Node :=
        (case Lib_Item_Or_Subunit.Kind is
           when Ada_Library_Item =>
             Ada_Node (F_Item (Library_Item (Lib_Item_Or_Subunit))),
           when Ada_Subunit =>
             Ada_Node (F_Body (Subunit (Lib_Item_Or_Subunit))),
         when others => raise Program_Error);

   --  Start of processing for Per_File_Action

   begin
      if Debug_Mode then
         Print (Unit);
--         Put ("With trivia\n");
--         PP_Trivia (Unit);
      end if;

      case Root_Node.Kind is
         when Ada_Package_Decl | Ada_Generic_Package_Decl |
           Ada_Subp_Decl | Ada_Generic_Subp_Decl =>
            if Arg (Cmd, Subunits) then
               Cmd_Error ("argument unit cannot have subunits");
            end if;
         when Ada_Body_Node =>
            if not Arg (Cmd, Subunits) then
               Cmd_Error ("input file looks like a body");
               --  ????We should not print the "try --help" message.
            end if;
         when others => raise Program_Error;
      end case;

      Generate (Tool, Cmd, File_Name, Input, BOM_Seen, Root_Node,
                Parent_Body_Of_Subunit => null);
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Stub_Tool) is
      pragma Unreferenced (Tool);
      use LAL_UL.Formatted_Output;
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Put ("Usage: gnatstub [options] filename [-cargs gcc_switches]\n");
      Put ("\n");
      Put ("  filename               Ada source file\n");
      Put ("\n");
      Put ("options:\n");
      Put ("  --version              Display version and exit\n");
      Put ("  --help                 Display usage and exit\n");
      Put ("\n");
      Put ("  -Pproject              Use project file project. Only one such switch.\n");
      Put ("                         can be used.\n");
      Put ("  -Xname=value           specify an external reference for argument\n");
      Put ("                         project file\n");
      Put ("  -eL                    follow all symbolic links when processing\n");
      Put ("                         project files\n");
      Put ("\n");
      Put (" --subunits              generate separate bodies for body stubs\n");
      Put ("\n");
      Put ("  -f                     replace an existing body file (if any), with a body\n");
      Put ("                         sample (is not allowed with '--subunits')\n");
      Put ("  -gnatec<path>          use additional configuration file,\n");
      Put ("                         same meaning as for gcc\n");
      Put ("  -gnatyMnnn             maximum line length in a sample body\n");
      Put ("  -gnatyn                (n in 1 .. 9) number of spaces used for indentation in\n");
      Put ("                         a sample body\n");
      Put ("  -gnatyo                alphabetically order local bodies\n");
      Put ("  -hg                    insert a sample comment header\n");
      Put ("  -hs                    insert the comment header from the spec\n");
      Put ("  --header-file=filename insert the comment header from the specified file\n");
      Put ("  -Idir                  source search dir, has the same meaning as for\n");
      Put ("                         gcc and gnatmake\n");
      Put ("  -I-                    do not look for the sources in the default directory\n");
      Put ("  -in                    same as -gnatyn\n");
      Put ("  -k                     do not remove the tree file\n");
      Put ("  -lnnn                  same as -gnatyMnnn\n");
      Put ("  --no-exception         avoid raising Program_Error in stubs\n");
      Put ("  --no-local-header      no local comment headers for unit stubs\n");
      Put ("  -o body-name           the name of the file to place the body into.\n");
      Put ("  --dir=directory        place generated file(s) into directory\n");
      Put ("  -W(h|u|s|e|8|b)        sets the wide character encoding of the result file\n");
      Put ("                          h - Hex ESC encoding\n");
      Put ("                          u - Upper half encoding\n");
      Put ("                          s - Shift-JIS encoding\n");
      Put ("                          e - EUC Encoding\n");
      Put ("                          8 - UTF-8 encoding\n");
      Put ("                          b - Brackets encoding (this is the default)\n");
      Put ("\n");
      Put ("  -q                     quiet mode\n");
      Put ("  -r                     reuse the tree file (if any) instead of creating it\n");
      Put ("                         (-r also implies -k)\n");
      Put ("  -t                     overwrite the existing tree file\n");
      Put ("  -v                     verbose mode\n");
      Put ("  gcc_switches           switches to be passed to gcc called by \1\n",
           Tool_Names.Tool_Name); -- ???
      Put ("\n");
      Put ("\n");
      Put ("Report bugs to report@adacore.com\n");

      pragma Style_Checks ("M79");
   end Tool_Help;

end Stub.Actions;

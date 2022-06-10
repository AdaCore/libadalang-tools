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

with Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding;
with System.WCh_Con;
with System.WCh_Cnv;
with Stub.Command_Lines; use Stub.Command_Lines;

with Ada.Directories; use Ada.Directories;
with Interfaces; use type Interfaces.Unsigned_16;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Common; use Libadalang.Common;
with LAL_Extensions;    use LAL_Extensions;

with Utils_Debug; use Utils_Debug;

with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
with Utils.Dbg_Out;
with Utils.Environment;
with Utils.Err_Out;
with Utils.Formatted_Output;
with Utils.Generic_Formatted_Output;
with Utils.Tool_Names;

with Pp.Actions;
with Pp.Command_Lines;

package body Stub.Actions is

   use Utils.Char_Vectors.Char_Vectors;

   function Image (X : Integer) return String
     renames Utils.String_Utilities.Image;

   use Common_Flag_Switches, Common_String_Switches;

   use Stub_Flag_Switches,
     Stub_String_Switches,
     Stub_Nat_Switches;

   use Scanner.Source_Message_Vectors;

   --  The current body-generating version of gnatstub generates output bodies;
   --  we could implement a new body-modifying version that modifies existing
   --  bodies.
   --
   --  Body-generating: By default, the command line arguments refer to specs,
   --  and the --subunits switch is not given. The output is a body. It is an
   --  error if the body already exists. If the --force/-f switch is given,
   --  then any existing body is overwritten; if that body contains
   --  hand-written code, it is lost. If the --subunits switch is given, the
   --  command line arguments refer to bodies, and a subunit is generated for
   --  each Ada stub found in that body, skipping ones that already exist.
   --
   --  Thus "--subunits switch given" if and only if "argument is a body".
   --
   --  Body-modifying: Command-line arguments always refer to specs. [???Or
   --  perhaps allow bodies, but process as if the corresponding spec were
   --  mentioned.] If the body exists, read it; otherwise create an empty one.
   --  Further processing can then assume that the body exists.
   --
   --  If --subunits is not given, then for each declaration in the spec and in
   --  the body that requires completion, and whose completion does not exist,
   --  generate the completion in the body. The completion is placed
   --  immediately after the completion of the preceding declaration (where the
   --  order takes into account the Alphabetical_Order switch, if given).
   --  What about private types???
   --
   --  If --subunits is given, then processing is as above, except we generate
   --  an Ada stub instead of a proper body.
   --
   --  With or without --subunits, for each Ada stub (whether or not newly
   --  generated), we generate the subunit if it doesn't already exist.
   --  ???Recursively process subunits, even if they already exist?
   --
   --  Withdraw support for the --force/-f switch. We are always updating an
   --  existing body, not replacing it.
   --
   --  Design notes:
   --
   --   Read text of spec, parse.
   --   Read text of body (or concoct empty one), parse, split text into lines.
   --   Collect relevant nodes into an Ada_Node_Vector. Relevant = requires
   --    completion, or is an Ada Stub. Nodes from the spec, followed by nodes
   --    from the body.
   --   Sort. By default, types first, then other things, otherwise retaining
   --   order from the source. If --alphabetical-order is given, then types
   --   first, in alphabetical order, then bodies, in alphabetical order,
   --   retaining source order for overloaded declarations.
   --
   --   Walk the sorted vector, generating a vector of strings. Each string is
   --   the text of a completion, and has a source location attached. For each
   --   item:
   --    If it requires completion, and the completion does not exist, append
   --    that completion to the string vector, along with the source location
   --    just after the preceding item. If --subunits is given, the completion
   --    is an Ada stub, and we also generate the subunit if it does not
   --    exist.
   --    If it is an Ada stub, generate the subunit if it does not exist.
   --
   --   Sort the completions in source location order.
   --
   --   Walk the text lines of the body and the completions in sync, copying
   --   both to the output, interleaved by source location.

   --  The above is way too complicated for now. A customer has asked for a way
   --  to generate the body for a single declaration in a package spec. Here
   --  are design notes for that. See Update_Body.
   --
   --  If --update=line is given, it must be a [generic] package spec.
   --  Collect subp decls.
   --  Find the one with the right sloc. Better not have a completion.
   --  Sort if --alphabetic-order.
   --  Find preceding one that has a completion, or the start of the
   --    declarative part if no such.
   --  Copy body text through that one.
   --  Append completion.
   --  Copy rest of body text.

   package Slocs renames Langkit_Support.Slocs;

   Elaborate_Body : constant Langkit_Support.Text.Unbounded_Text_Type :=
     Langkit_Support.Text.To_Unbounded_Text ("elaborate_body");

   function Has_Elaborate_Body (N : Ada_Node) return Boolean is
     (Exists (P_Get_Aspect (N.As_Basic_Decl, Elaborate_Body)));

   ----------
   -- Init --
   ----------

   procedure Init
     (Tool : in out Stub_Tool; Cmd : in out Command_Line) is
      pragma Unreferenced (Tool);
   begin
      --  ????Other checks from gnatstub/lal_ul-check_parameters.adb?

      pragma Assert (Environment.Initial_Dir = Current_Directory);

      if Update_Body_Specified (Cmd) and then Num_File_Names (Cmd) > 1 then
         Cmd_Error
           ("only one file name allowed with " &
            Switch_Text
              (Stub.Command_Lines.Descriptor, To_All (Update_Body)).all);
      end if;

      --  Note that we never call Pp.Actions.Pp_Tool.Init
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
      use Utils.Dbg_Out;
   begin
      Utils.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure psloc (X : Ada_Node) is

      function Lines_String
        (Sloc_Range : Slocs.Source_Location_Range) return String is
         (Image (Integer (Sloc_Range.Start_Line)) & ": " &
          Image (Integer (Sloc_Range.End_Line)));

      use Utils.Dbg_Out;
   begin
      Utils.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", Lines_String (Sloc_Range (X)));
   end psloc;

   procedure nn (X : Ada_Node) is
      use Utils.Dbg_Out;
   begin
      Utils.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X.Is_Null then "null" else X.Image));
   end nn;

   procedure ppp (X : Ada_Node) is
      use Utils.Dbg_Out;
   begin
      nn (X);
      Print (X);
   end ppp;

   procedure Put_Ada_Node_Array (X : Ada_Node_Array) is
      use Utils.Dbg_Out;
   begin
      for N of X loop
         nn (N);
         Put ("----------------\n");
      end loop;
   end Put_Ada_Node_Array;

   procedure Put_Child_Record (C : Child_Record) is
      use Utils.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", C.Node.Image);
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
      use Utils.Dbg_Out;
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
      use Utils.Formatted_Output;
   begin
      if Debug_Flag_V then
         Put ("\1\n", Message);
      end if;
   end Dump;
   pragma Style_Checks (On);
   pragma Warnings (On);

   procedure Generate_File
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Root_Node : Ada_Node;
      Parent_Body_Of_Subunit : Ada_Node);
   --  Given a spec, generate the body file. Given a body, recursively call
   --  Generate_File on any Ada stubs, and given a stub, generate the subunit
   --  file.
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

   Syntax_Only_Arg : aliased String := "--syntax-only";
   --  --syntax-only makes it easier to debug, because we don't get so many
   --  Property_Errors.
   Comments_Fill_Arg : aliased String := "--comments-fill";
   Decimal_Grouping_Arg : aliased String := "--decimal-grouping=3";
   Based_Grouping_Arg : aliased String := "--based-grouping=4";

   Args : aliased GNAT.OS_Lib.Argument_List :=
     [Syntax_Only_Arg'Access,
      Comments_Fill_Arg'Access,
      Decimal_Grouping_Arg'Access,
      Based_Grouping_Arg'Access];

   function Get_Pp_Cmd return Command_Line is
   begin
      return Result : Command_Line (Pp.Command_Lines.Descriptor'Access) do
         Parse (Args'Access, Result,
                Cmd_Line_1, Null_Callback'Access,
                Collect_File_Names => False);
      end return;
   end Get_Pp_Cmd;

   Pp_Base_Cmd : constant Cmd_Line := Get_Pp_Cmd;

   function Overriding_String
     (Overrides : Ada_Overriding_Node) return W_Str is
      (case Overrides is
         when Ada_Overriding_Not_Overriding => "not overriding ",
         when Ada_Overriding_Overriding => "overriding ",
         when Ada_Overriding_Unspecified => "");

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
      function Recurse (Decls : Ada_Node_List) return Boolean;
      --  Recursively process a nested declaration list. This is used for a
      --  package spec, which needs a completion if and only if there's
      --  something in it that needs a completion.

      function Recurse (Decls : Ada_Node_List) return Boolean is
      begin
         if not Decls.Is_Null then
            for X in 1 .. Last_Child_Index (Decls) loop
               declare
                  Subtree : constant Ada_Node := Childx (Decls, X);
               begin
                  if Needs_Completion (Subtree)
                    or else Subtree.Kind in Ada_Incomplete_Type_Decl |
                                            Ada_Incomplete_Tagged_Type_Decl
                  then
                     return True;
                  end if;
               end;
            end loop;
         end if;

         return False;
      end Recurse;

   --  Start of processing for Needs_Completion

   begin
      case N.Kind is
         when Ada_Package_Decl | Ada_Generic_Package_Decl =>
            if Has_Elaborate_Body (N) then
               return True;
            end if;

            declare
               VP : constant Public_Part := Vis_Part (N);
               PP : constant Private_Part := Priv_Part (N);
            begin
               return
                 (not VP.Is_Null and then Recurse (F_Decls (VP)))
                   or else
                 (not PP.Is_Null and then Recurse (F_Decls (PP)));
            end;

         when Ada_Single_Protected_Decl | Ada_Protected_Type_Decl |
              Ada_Single_Task_Decl | Ada_Task_Type_Decl =>
            return True;

         when Ada_Entry_Decl =>
            return True;

         when Ada_Subp_Decl =>
            --  For a function defined in the public part of a package spec and
            --  implemented in the private part as an expression function,
            --  the P_Next_Part_For_Decl should return a non-null value
            --  and no need to generate a stub for it in the body.
            --  (see ticket for a more detailed use case T619-029)

            if not N.As_Basic_Subp_Decl.P_Next_Part_For_Decl.Is_Null then
               declare
                  Paren_D_Scope : constant Basic_Decl :=
                    N.P_Declarative_Scope.P_Parent_Basic_Decl;
                  Paren_B_Scope : constant Basic_Decl :=
                    N.As_Basic_Subp_Decl.P_Next_Part_For_Decl.
                      P_Declarative_Scope.P_Parent_Basic_Decl;

                  pragma Assert (Paren_D_Scope.Kind = Ada_Package_Decl);
               begin
                  --  If the body part for declaration has the same parent
                  --  scope (expected, Package_Decl) no need to generate a
                  --  stub in this case.
                  --  However, if a stub was already been generated the parent
                  --  scope won't be the same kind as for the declaration and
                  --  in this case, if requested, we should be able to
                  --  re-generated.

                  if Paren_D_Scope.Kind = Paren_B_Scope.Kind then
                     return False;
                  end if;
               end;
            end if;

            return not N.As_Basic_Subp_Decl.P_Is_Imported;

         when Ada_Generic_Subp_Decl =>
            return not N.As_Generic_Subp_Decl.P_Is_Imported;

         when Ada_Incomplete_Type_Decl | Ada_Incomplete_Tagged_Type_Decl =>
            return False;

            --  Because these are handled specially in Walk

         when others =>
            return False;
      end case;
   end Needs_Completion;

   function Get_Parent_Name (Parent_Body_Of_Subunit : Ada_Node) return W_Str is
   begin
      if Parent_Body_Of_Subunit.Is_Null then
         return "";
      elsif Ada_Node'(Parent (Parent_Body_Of_Subunit)).Kind = Ada_Subunit then
         declare
            Parent_Parent : constant W_Str :=
              Full_Name (Parent_Body_Of_Subunit.Parent.As_Subunit.F_Name);
            Parent_Simple : constant W_Str :=
              Id_Name (Get_Def_Name (Parent_Body_Of_Subunit));
         begin
            return Parent_Parent & "." & Parent_Simple;
         end;
      else
         return Full_Name (Get_Def_Name (Parent_Body_Of_Subunit).As_Name);
      end if;
   end Get_Parent_Name;

   procedure Generate_File
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Root_Node : Ada_Node;
      Parent_Body_Of_Subunit : Ada_Node)
   is
      Generating_Ada_Stubs : constant Boolean :=
        Root_Node.Kind in Ada_Package_Decl | Ada_Generic_Package_Decl
        and then Arg (Cmd, Subunits);
      --  True if gnatstub is called on a [generic] package spec with the
      --  --subunits switch. In this case, we generate a corresponding package
      --  body containing Ada stubs for the subprograms in the spec. Then we
      --  call Process_File recursively on the newly-generated package body to
      --  generate subunits.

      Looking_For_Ada_Stubs : constant Boolean :=
        Root_Node.Kind in Ada_Body_Node
        and then Root_Node.Kind not in Ada_Body_Stub;
      Parent_Name : constant W_Str := Get_Parent_Name (Parent_Body_Of_Subunit);
      Root_Node_Name : constant W_Str :=
        (if not Parent_Body_Of_Subunit.Is_Null
         then Parent_Name & "." else "") &
        Full_Name (Get_Def_Name (Root_Node).As_Name);
      UC_Root_Node_Name : constant W_Str := To_Upper (Root_Node_Name);
      LC_Root_Node_Name : constant W_Str := To_Lower (Root_Node_Name);

      Wide_Char_Encoding : constant System.WCh_Con.WC_Encoding_Method :=
        Wide_Character_Encoding (Cmd);
      Out_Vec, Pp_Out_Vec : Char_Vector;

      Pp_Cmd : Cmd_Line := Copy_Command_Line (Pp_Base_Cmd);
      --  Make a local copy so we can modify it per file

      function Get_EOL_Switch return String;
      --  Return "lf" or "crlf" according to which end-of-line convention the
      --  input uses. We want the output to match the input.

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

      package Buffered_Output is new Utils.Generic_Formatted_Output
        (W_Char,
         W_Str,
         Basic_Put_Char => Put_To_Out_Vec);
      use Buffered_Output;

      procedure Generate_CU_Header;

      procedure Walk (Decl : Ada_Node; Level : Natural);
      --  Generate code corresponding to Decl, and recursively walk subtrees.
      --
      --  Note on recursion: Generate_File calls Walk. Walk calls Walk. Walk
      --  calls Generate_File. In more detail: Generate_File is called on the
      --  compilation unit spec or body. In the case of a spec, Generate_File
      --  calls Walk on the spec, which calls Walk on nested specs, and
      --  generates a body for each one. In the case of a body, Generate_File
      --  calls Walk on the body, which calls Generate_File on nested Ada body
      --  stubs, to generate subunit files for each one.

      procedure Generate_Local_Header (Name : W_Str; Level : Natural);
      --  Generate the local header that appears before each body,
      --  unless the --no_local_header switch was given.

      procedure Generate_Subunit_Start (Level : Natural);
      --  If we are processing a subunit, generate "separate (parent)".

      procedure Generate_Subp_Body
        (Decl : Ada_Node; Name : W_Str; Ada_Stub : Boolean);
      --  Generate a subprogram body stub. If Ada_Stub is True, we generate
      --  "is separate"; otherwise the so-called "stub" is a proper body.

      procedure Generate_Entry_Body (Decl : Ada_Node; Name : W_Str);
      --  Generate an entry body stub

      procedure Generate_Subp_Or_Entry_Body
        (Decl : Ada_Node; Name : W_Str; Ada_Stub : Boolean);

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

      procedure Process_New_Body;
      --  Recursively call Process_File on the body file. This is used after
      --  Update_Body has updated the body, and also in case the command-line
      --  arg is a spec, and the --subunits switch was given, so we generated a
      --  body containing Ada stubs.

      procedure Update_Body;
      --  Implement the --update-body=N switch.

      procedure Generate_CU_Header is
      begin
         if Arg (Cmd, Header_File) /= null then
            declare
               Header : String_Access :=
                 Read_File (Arg (Cmd, Header_File).all);
            begin
               Put ("\1", From_UTF8 (Header.all));
               Free (Header);
            end;
         end if;

         if Arg (Cmd, Comment_Header_Sample) then
            declare
               With_Spaces : constant W_Str :=
                 Intersperse_Spaces (UC_Root_Node_Name);
               Len : constant Natural := Arg (Cmd, Max_Line_Length);
               Dashes : constant W_Str := [1 .. Len => '-'];
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
               --  noncomment line. Set Last to point to the NL at the end
               --  of the last comment line in the header.

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
               Header_Line : constant W_Str := [1 .. Header_Length => '-'];
            begin
               Put (" \1\n", Header_Line);
               Put (" -- \1 --\n", Name);
               Put (" \1\n\n", Header_Line);
            end;
         end if;
      end Generate_Local_Header;

      procedure Generate_Subunit_Start (Level : Natural) is
      begin
         if Level = 0 and then not Parent_Body_Of_Subunit.Is_Null then
            Put ("separate (\1)\n", Parent_Name);
         end if;
      end Generate_Subunit_Start;

      procedure Generate_Subp_Body
        (Decl : Ada_Node; Name : W_Str; Ada_Stub : Boolean)
      is
         Empty_Vec, Pp_Out_Vec : Char_Vector;
         Spec : constant Subp_Spec := Get_Subp_Spec (Decl);
         Overrides : constant Ada_Overriding_Node :=
           (if Decl.Kind in Ada_Classic_Subp_Decl
              then Decl.As_Classic_Subp_Decl.F_Overriding
              else Ada_Overriding_Unspecified);
         Returns : constant Boolean :=
           not F_Subp_Returns (Spec).Is_Null;
      begin
         Pp.Actions.Format_Vector
           (Pp_Cmd,
            Input => Empty_Vec,
            Node => Ada_Node (Spec),
            Output => Pp_Out_Vec,
            Messages => Tool.Ignored_Messages);
         pragma Assert (Is_Empty (Tool.Ignored_Messages));
         Put (" \1\2 is",
              Overriding_String (Overrides),
              From_UTF8
                (Elems (Pp_Out_Vec) (1 .. Last_Index (Pp_Out_Vec))));
         if Ada_Stub then
            Put (" separate;\n");
         else
            Put ("\n");
            Generate_Stub_Begin_End
              (Name, (if Returns then "function" else "procedure"));
         end if;
      end Generate_Subp_Body;

      procedure Generate_Entry_Body (Decl : Ada_Node; Name : W_Str) is
         Empty_Vec, Pp_Out_Vec : Char_Vector;
         Parms : constant Params :=
           Decl.As_Entry_Decl.F_Spec.F_Entry_Params;
         Overrides : constant Ada_Overriding_Node :=
           Decl.As_Entry_Decl.F_Overriding;
      begin
         if not Parms.Is_Null then
            Pp.Actions.Format_Vector
              (Pp_Cmd,
               Input => Empty_Vec,
               Node => Ada_Node (Parms),
               Output => Pp_Out_Vec,
               Messages => Tool.Ignored_Messages);
            pragma Assert (Is_Empty (Tool.Ignored_Messages));
            Put ("\1entry \2 \3 when Standard.True is\n",
                 Overriding_String (Overrides),
                 Name,
                 From_UTF8
                   (Elems (Pp_Out_Vec)
                      (1 .. Last_Index (Pp_Out_Vec))));
         else
            Put ("\1entry \2 when Standard.True is\n",
                 Overriding_String (Overrides),
                 Name);
         end if;

         Generate_Stub_Begin_End (Name, "entry");
      end Generate_Entry_Body;

      procedure Generate_Subp_Or_Entry_Body
        (Decl : Ada_Node; Name : W_Str; Ada_Stub : Boolean) is
      begin
         case Decl.Kind is
            when Ada_Subp_Decl | Ada_Generic_Subp_Decl =>
               Generate_Subp_Body (Decl, Name, Ada_Stub);
            when Ada_Entry_Decl =>
               Generate_Entry_Body (Decl, Name);
            when others =>
               raise Program_Error;
         end case;
      end Generate_Subp_Or_Entry_Body;

      procedure Generate_Stub_Begin_End (Name, Stub_Kind : W_Str) is
         Returns : constant Boolean := Stub_Kind = "function";
      begin
         Put ("begin\n");
         Put ("pragma Compile_Time_Warning " &
                "(Standard.True, ""\1 unimplemented"");\n",
              Q (Name));

         --  If --no-exception was specified, we suppress the "raise", except
         --  we can't do that for functions, because they require a return
         --  statement.

         if not Arg (Cmd, No_Exception) or else Returns then
            --  For a procedure, generate a raise statement. For a function,
            --  generate a return statement containing a raise expression.

            Put ("\1raise Program_Error with ""Unimplemented \2 \3"";\n",
                 (if Returns then "return " else ""), Stub_Kind, Q (Name));
         end if;

         Put ("end;\n");
      end Generate_Stub_Begin_End;

      procedure Walk (Decl : Ada_Node; Level : Natural) is
         Local_Decls, Incomplete_Types : Ada_Node_Vector;
         Name : constant W_Str := Full_Name (Get_Def_Name (Decl).As_Name);
         use Ada_Node_Vectors;

         procedure Collect_Local_Decls (Decls : Ada_Node_List);
         --  Append all the declarations that need recursive processing onto
         --  Local_Decls.

         procedure Collect_Incomplete_Types (Decls : Ada_Node_List);
         --  Append all the incomplete type declarations in a private part that
         --  are not completed in the same private part onto Incomplete_Types.

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

         procedure Collect_Incomplete_Types (Decls : Ada_Node_List) is
         begin
            for X in 1 .. Last_Child_Index (Decls) loop
               declare
                  Subtree : constant Ada_Node := Childx (Decls, X);
               begin
                  if Subtree.Kind in Ada_Incomplete_Type_Decl |
                    Ada_Incomplete_Tagged_Type_Decl
                  then
                     declare
                        Next_Part : constant Base_Type_Decl :=
                          Subtree.As_Base_Type_Decl.P_Next_Part;
                     begin
                        if Next_Part.Is_Null
                          or else Next_Part = Subtree
                        then
                           --  ???Not sure why P_Next_Part sometimes returns
                           --  "self".

                           Append (Incomplete_Types, Subtree);
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end Collect_Incomplete_Types;

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
               Generate_Subunit_Start (Level);
               Put ("package body \1 is\n", Name);
            when Ada_Single_Protected_Decl | Ada_Protected_Type_Decl |
              Ada_Protected_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start (Level);
               Put ("protected body \1 is\n", Name);
            when Ada_Single_Task_Decl | Ada_Task_Type_Decl |
              Ada_Task_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start (Level);
               Put ("task body \1 is\n", Name);
               Generate_Stub_Begin_End (Name, "task");

            when Ada_Subp_Decl | Ada_Generic_Subp_Decl | Ada_Subp_Body_Stub =>
               Generate_Local_Header (Name, Level);
               Generate_Subunit_Start (Level);
               Generate_Subp_Body
                 (Decl, Name, Ada_Stub => Generating_Ada_Stubs);

            when Ada_Entry_Decl =>
               Generate_Local_Header (Name, Level);
               Generate_Entry_Body (Decl, Name);

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
               if not Vis_Part (Decl).Is_Null then
                  Collect_Local_Decls (F_Decls (Vis_Part (Decl)));
               end if;

               if not Priv_Part (Decl).Is_Null then
                  Collect_Local_Decls (F_Decls (Priv_Part (Decl)));
               end if;
            when Ada_Subp_Body | Ada_Package_Body | Ada_Task_Body |
              Ada_Protected_Body =>
               Collect_Local_Decls (F_Decls (Body_Decls (Decl)));

            when Ada_Package_Body_Stub | Ada_Protected_Body_Stub =>
               --  Find the corresponding spec, and walk the decls therein

               declare
                  Spec : constant Basic_Decl :=
                    P_Previous_Part (Decl.As_Body_Node);
               begin
                  if not Vis_Part (Spec).Is_Null then
                     Collect_Local_Decls (F_Decls (Vis_Part (Spec)));
                  end if;

                  if not Priv_Part (Spec).Is_Null then
                     Collect_Local_Decls (F_Decls (Priv_Part (Spec)));
                  end if;
               end;

            when Ada_Subp_Decl | Ada_Generic_Subp_Decl |
              Ada_Single_Task_Decl | Ada_Task_Type_Decl |
              Ada_Entry_Decl |
              Ada_Task_Body_Stub | Ada_Subp_Body_Stub =>
               null;

            when others => raise Program_Error;
         end case;

         --  Sort the list if appropriate. There's no point in sorting if we're
         --  doing subunits, because each one goes in a separate file.

         if Arg (Cmd, Alphabetical_Order) and not Looking_For_Ada_Stubs then
            Sorting.Sort (Local_Decls);
         end if;

         --  For an incomplete type in a private part that has no completion in
         --  the same private part, generate a completion in the package body.
         --  We do this separately from the normal walk, because we want the
         --  full type declarations to come first, before procedure bodies and
         --  whatnot.

         case Decl.Kind is
            when Ada_Package_Decl | Ada_Generic_Package_Decl =>
               if not Priv_Part (Decl).Is_Null then
                  Collect_Incomplete_Types (F_Decls (Priv_Part (Decl)));

                  for Child of Incomplete_Types loop
                     Put ("\ntype \1 is\2 null record;\n",
                          Full_Name (Get_Def_Name (Child).As_Name),
                          (if Child.Kind = Ada_Incomplete_Type_Decl then ""
                          else " tagged"));
                  end loop;
               end if;

            when others => null;
         end case;

         --  Recursively process the nested declarations. In the case of Ada
         --  stubs, we call Generate_File, because the corrsponding subunit
         --  goes in a separate file, and Generate_File knows how to create
         --  files.

         for Child of Local_Decls loop
            if Looking_For_Ada_Stubs and
              Decl.Kind not in Ada_Package_Body_Stub | Ada_Protected_Body_Stub
            then
               pragma Assert (Child.Kind in Ada_Body_Stub);
               Generate_File
                 (Tool, Cmd, File_Name, Input, BOM_Seen,
                  Root_Node => Child,
                  Parent_Body_Of_Subunit => Root_Node);
            else
               Walk (Child, Level + 1);
            end if;
         end loop;

         --  For things we recursively walked above (packages, protecteds),
         --  generate the end of the body. For other things (tasks,
         --  subprograms, entries), generate nothing.

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
              Ada_Protected_Body =>
               null;
            when others => raise Program_Error;
         end case;
      end Walk;

      procedure Format is
         Context : constant Analysis_Context :=
           Create_Context (Charset => Wide_Character_Encoding (Cmd));
         Out_Str : String renames Elems (Out_Vec) (1 .. Last_Index (Out_Vec));
         Out_Unit : constant Analysis_Unit := Get_From_Buffer
           (Context, Filename => "", Buffer => Out_Str);
      begin
         if Has_Diagnostics (Out_Unit) then
            if Assert_Enabled then
               Text_IO.Put_Line ("Errors while parsing """ & Out_Str & """");
               for D of Libadalang.Analysis.Diagnostics (Out_Unit) loop
                  Text_IO.Put_Line
                    (Langkit_Support.Diagnostics.To_Pretty_String (D));
               end loop;
            end if;

            raise Program_Error;
         end if;
         pragma Assert (not Root (Out_Unit).Is_Null);

         Pp.Actions.Format_Vector
           (Pp_Cmd,
            Input => Out_Vec,
            Node => Root (Out_Unit),
            Output => Pp_Out_Vec,
            Messages => Tool.Ignored_Messages);
         pragma Assert (Is_Empty (Tool.Ignored_Messages));
      end Format;

      function Get_Output_Name (Resolve_Links : Boolean) return String is
         pragma Unreferenced (Resolve_Links);

         use GNATCOLL.Projects, GNATCOLL.VFS;

         function Default_Name return String;
         --  This is used when the output file is not specified on the
         --  command line, and there is no project file. It uses the
         --  default GNAT conventions for file names (.ads, .adb).

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
            Unit_Name : constant String := To_UTF8 (LC_Root_Node_Name);
            --  Unit_Name is correct for both library units and Ada stubs
            Part : constant Unit_Parts :=
              (if Root_Node.Kind in Ada_Body_Stub then Unit_Separate
               else Unit_Body);
         begin
            pragma Assert
              (Extending_Project (Project (Arg_File_Info)) = No_Project);
            --  We don't want to modify extended projects

            return Result : constant String :=
              +GNATCOLL.Projects.File_From_Unit
                (Project         => Project (Arg_File_Info),
                 Unit_Name       => Unit_Name,
                 Part            => Part,
                 Language        => "Ada",
                 File_Must_Exist => False)
            do
               null;
            end return;
         end Name_From_Project;

         --  If the output file is specified on the command line,
         --  use that. Otherwise, if there is a project file,
         --  use that. Otherwise use the default naming convention.

         Simple : constant String :=
           (if Arg (Cmd, Output) = null then
              (if Status (Tool.Project_Tree.all) = Empty
                 then Simple_Name (Default_Name)
                 else Name_From_Project)
            else Arg (Cmd, Output).all);
         pragma Assert (Simple_Name (Simple) = Simple);
      begin
         return Result : constant String :=
           (if Arg (Cmd, Output_Directory) = null
              then Compose (Containing_Directory (File_Name), Simple)
              else Compose (Arg (Cmd, Output_Directory).all, Simple))
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
            Write_File (Out_File, Ada.Strings.UTF_Encoding.BOM_8);
         end if;

         Write_File (Out_File, Out_String);

         Close (Out_File, Status);
         if not Status then
            raise Program_Error with
              "write of " & Output_Name & " failed";
         end if;

         if not Arg (Cmd, Quiet) then
            Formatted_Output.Put
              ("\1 \2\n", Simple_Name (Output_Name),
               (if Update_Body_Specified (Cmd) then "updated" else "created"));
         end if;

         if Debug_Flag_C then
            Formatted_Output.Put ("wrote \1\n", Output_Name);
         end if;
      end Write_Output_File;

      procedure Process_New_Body is
         Body_Cmd       : Cmd_Line := Copy_Command_Line (Cmd);
         Has_Syntax_Err : Boolean := False;
      begin
         Clear_File_Names (Body_Cmd);
         Append_File_Name (Body_Cmd, Output_Name);
         Set_Arg (Body_Cmd, Update_Body, No_Update_Body);

         --  We pass Reparse => True to Process_File, because otherwise (for
         --  Update_Body), it will think the body file has not changed, and
         --  will incorrectly "optimize" away the work. In the other case
         --  (Subunits), the body is new, so Reparse doesn't matter.

         Process_File
           (Tool, Body_Cmd, Output_Name, Counter => 1,
            Syntax_Error => Has_Syntax_Err,
            Reparse => True);
      end Process_New_Body;

      procedure Update_Body is
         use Slocs, Ada_Node_Vectors;
         Search_Line : constant Line_Number :=
           Line_Number (Arg (Cmd, Update_Body));
         --  For --update-body=N, this is the value of N; we are searching for
         --  a subprogram declaration that appears on line N of the spec.

         Subp_Decl : Ada_Node := No_Ada_Node;
         --  The Subp_Decl at Search_Line

         Body_Line : Line_Number;
         --  The line number in the body file after which the new subprogram
         --  body should be inserted.
         Body_Line_Set : Boolean := False;

         procedure Search;
         --  Compute Subp_Decl and Body_Line

         function Find_Insertion_Index
           (Old_Content : String; Body_Line : Line_Number) return Positive;
         --  Return the index at which the stub is to be inserted

         procedure Indent_Stub (Amount : Natural);
         --  Indents the stub that is in Pp_Out_Vec. This is needed because the
         --  --initial-indentation switch doesn't fully work. We don't specify
         --  --initial-indentation=3; instead we subtract 3 from
         --  --max-line-length, and call Indent_Stub.

         procedure Search is
            --  Walk the spec, recursing into nested package and protected
            --  specs, searching for the subprogram declaration at Search_Line.
            --  Each time we see a declaration that has a corresponding
            --  declaration in the body, update Body_Line; we're going to
            --  insert the new body after the last such one encountered. When
            --  we find the subprogram we're searching for, we set Subp_Decl
            --  and quit.

            procedure Rec (Decl : Ada_Node);

            procedure Rec (Decl : Ada_Node) is
               Sorted : Ada_Node_Vector;

               procedure Update_Body_Line (New_Body_Line : Line_Number);
               --  Update Body_Line, asserting that we have not yet found the
               --  subprogram declaration we are looking for; once found, we
               --  should quit.

               procedure Collect_Decls (Decls : Ada_Node_List);
               --  Collect the declarations into Sorted, which will be sorted
               --  if --alphabetical-order was specified.

               procedure Update_Body_Line (New_Body_Line : Line_Number) is
                  use Utils.Formatted_Output;
               begin
                  if Debug_Flag_V then
                     nn (Decl);
                     Put ("\1 ==> \2\n", Image (Integer (Body_Line)),
                          Image (Integer (New_Body_Line)));
                  end if;
                  pragma Assert (Subp_Decl.Is_Null);
                  Body_Line_Set := True;
                  Body_Line := New_Body_Line;
               end Update_Body_Line;

               procedure Collect_Decls (Decls : Ada_Node_List) is
               begin
                  for X in 1 .. Last_Child_Index (Decls) loop
                     declare
                        Subtree : constant Ada_Node := Childx (Decls, X);
                     begin
                        Append (Sorted, Subtree);
                     end;
                  end loop;
               end Collect_Decls;

               Sloc : constant Source_Location_Range := Sloc_Range (Decl);
               Decl_Name : constant String :=
                 To_UTF8 (Full_Name (Get_Def_Name (Decl).As_Name));

            --  Start of processing for Rec

            begin
               if Search_Line in Sloc.Start_Line .. Sloc.End_Line then
                  case Decl.Kind is
                     when Ada_Package_Decl | Ada_Generic_Package_Decl |
                       Ada_Single_Protected_Decl | Ada_Protected_Type_Decl =>
                        declare
                           B : constant Body_Node :=
                             Decl.As_Basic_Decl.P_Body_Part_For_Decl;
                        begin
                           if B.Is_Null then
                              if not Body_Line_Set then
                                 Cmd_Error
                                   ("body of " & Decl_Name & " not found");
                              end if;
                           else
                              Update_Body_Line
                                (Body_Decls (B).Sloc_Range.Start_Line);
                           end if;
                        end;

                        if not Vis_Part (Decl).Is_Null then
                           Collect_Decls (F_Decls (Vis_Part (Decl)));
                        end if;

                        if not Priv_Part (Decl).Is_Null then
                           Collect_Decls (F_Decls (Priv_Part (Decl)));
                        end if;

                        if Arg (Cmd, Alphabetical_Order) then
                           Sorting.Sort (Sorted);
                        end if;

                        for X in 1 .. Last_Index (Sorted) loop
                           Rec (Sorted (X));
                           exit when not Subp_Decl.Is_Null;
                        end loop;
                        if Subp_Decl.Is_Null then
                           Cmd_Error ("subprogram not found at line " &
                                        Image (Arg (Cmd, Update_Body)));
                        end if;

                     when Ada_Subp_Decl | Ada_Generic_Subp_Decl |
                       Ada_Entry_Decl =>
                        pragma Assert (Subp_Decl.Is_Null);
                        Subp_Decl := Decl;

                        if not Decl.As_Basic_Decl.P_Body_Part_For_Decl.Is_Null
                        then
                           Cmd_Error
                             ("body for " & Decl_Name & " already exists");
                        end if;

                     when others => null;
                  end case;

               else
                  declare
                     B : constant Body_Node :=
                       Decl.As_Basic_Decl.P_Body_Part_For_Decl;
                  begin
                     if not B.Is_Null then
                        Update_Body_Line (B.Sloc_Range.End_Line);
                     end if;
                  end;
               end if;
            end Rec;

         begin
            Rec (Root_Node);
            if Subp_Decl.Is_Null then
               Cmd_Error ("no subprogram found at line " &
                            Image (Arg (Cmd, Update_Body)));
            end if;
         end Search;

         function Find_Insertion_Index
           (Old_Content : String; Body_Line : Line_Number) return Positive
         is
            Line_Num : Line_Number := 1;
         begin
            return Result : Positive := 1 do
               while Line_Num <= Body_Line loop
                  if Old_Content (Result) = ASCII.LF then
                     Line_Num := Line_Num + 1;
                  end if;
                  Result := Result + 1;
               end loop;
            end return;
         end Find_Insertion_Index;

         procedure Indent_Stub (Amount : Natural) is
            Ind : constant String := [1 .. Amount => ' '];
            Temp : Char_Vector;
         begin
            Append (Temp, Ind);

            for X in 1 .. Last_Index (Pp_Out_Vec) loop
               Append (Temp, Pp_Out_Vec (X));

               if Pp_Out_Vec (X) = ASCII.LF
                 and then X /= Last_Index (Pp_Out_Vec)
                 and then Pp_Out_Vec (X + 1) /= ASCII.LF
               then
                  Append (Temp, Ind);
               end if;
            end loop;

            Move (Target => Pp_Out_Vec, Source => Temp);
         end Indent_Stub;

         Old_Content : String_Access := Read_File (Output_Name);
         Backup_Name : constant String := Output_Name & ".bak";
         Create_Backup : constant Boolean := False;
         --  For now, we do not create a backup file

      --  Start of processing for Update_Body

      begin
         if Root_Node.Kind not in
           Ada_Package_Decl | Ada_Generic_Package_Decl
         then
            Cmd_Error ("package spec not found");
         end if;

         Search;
         pragma Assert (Body_Line_Set);

         declare
            Name : constant W_Str :=
              Full_Name (Get_Def_Name (Subp_Decl).As_Name);
            Level : constant Natural := 1;
            Insertion_Index : constant Positive :=
              Find_Insertion_Index (Old_Content.all, Body_Line);
            pragma Assert (Old_Content (Insertion_Index - 1) = ASCII.LF);
            use Pp.Command_Lines, Pp.Command_Lines.Pp_Nat_Switches;
         begin
            --  We want to insert the generated stub into the body at
            --  Insertion_Index. We want to pretty print the generated stub,
            --  but leave the rest of the code alone. So we generate the stub
            --  into Out_Vec, then call Format, which formats it into
            --  Pp_Out_Vec. Then move it back into Out_Vec, copy the first part
            --  of the body to Pp_Out_Vec, copy the stub into Pp_Out_Vec, and
            --  copy the rest of the body into Pp_Out_Vec. Finally, call
            --  Write_Output_File to write Pp_Out_Vec to the output (body)
            --  file.

            pragma Assert (Is_Empty (Out_Vec) and Is_Empty (Pp_Out_Vec));
            Generate_Local_Header (Name, Level);
            Generate_Subunit_Start (Level);
            Generate_Subp_Or_Entry_Body
              (Subp_Decl, Name, Ada_Stub => Arg (Cmd, Subunits));
            Set_Arg (Pp_Cmd, Initial_Indentation, 0);

            declare
               Switch : constant Pp_Nats := Pp.Command_Lines.Max_Line_Length;
               Val : constant Natural := Arg (Pp_Cmd, Switch);
            begin
               Set_Arg (Pp_Cmd, Switch, Val - 3);
            end;

            if Arg (Cmd, Subunits) then
               --  We would prefer to use Format in this case, with an
               --  appropriate Rule passed to Get_From_Buffer, but that
               --  doesn't quite work.

               Move (Target => Pp_Out_Vec, Source => Out_Vec);
               Indent_Stub (2);
            else
               Format;
               Indent_Stub (3);
            end if;
            Move (Target => Out_Vec, Source => Pp_Out_Vec);

            Append (Pp_Out_Vec,
                    Old_Content (Old_Content'First .. Insertion_Index - 1));
            Append (Pp_Out_Vec, ASCII.LF);
            Append (Pp_Out_Vec, Elems (Out_Vec) (1 .. Last_Index (Out_Vec)));
            if Old_Content (Insertion_Index) /= ASCII.LF then
               Append (Pp_Out_Vec, ASCII.LF);
            end if;
            Append (Pp_Out_Vec,
                    Old_Content (Insertion_Index .. Old_Content'Last));
            Free (Old_Content);
            Clear (Out_Vec);
         end;

         if Create_Backup then
            Move_File (Old_Name => Output_Name, New_Name => Backup_Name);
         end if;
         Write_Output_File;

         --  Finally, if we generated an Ada stub (i.e. --subunits is True), we
         --  recursively call ourself on the body to generate the subunits.

         if Arg (Cmd, Subunits) then
            Process_New_Body;
         end if;
      end Update_Body;

      function Get_EOL_Switch return String is
      begin
         --  If all line endings are CRLF, we return "crlf"; if all are LF, we
         --  return "lf". If the file is malformed (mixed, CR without LF, no
         --  line endings at all), we don't care -- we arbitrarily return "lf"
         --  in those cases, so we don't have to search past the first CR or
         --  LF.

         for Ch of Input loop
            case Ch is
               when ASCII.CR => return "crlf";
               when ASCII.LF => exit;
               when others => null;
            end case;
         end loop;

         return "lf";
      end Get_EOL_Switch;

      EOL_Switch : constant String := Get_EOL_Switch;

      use Pp.Command_Lines, Pp.Command_Lines.Pp_String_Switches;

   --  Start of processing for Generate_File

   begin
      Set_Arg (Pp_Cmd, End_Of_Line, EOL_Switch);

      if Update_Body_Specified (Cmd) then
         Update_Body;
         goto Skip;
      end if;

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
      --  rubbage, which we ignore. Looking_For_Ada_Stubs will be False if
      --  the input is a spec, so we're generating bodies, or if we're in a
      --  recursive call to Generate_File for an Ada stub. It will be True
      --  only for the outer call with a body as input.

      if not Looking_For_Ada_Stubs then
         Format;
         Write_Output_File;
      end if;

      if Generating_Ada_Stubs then
         Process_New_Body;
      end if;

      <<Skip>>
   end Generate_File;

   procedure Per_File_Action
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
   is
      Lib_Item_Or_Subunit : constant Ada_Node :=
        Root (Unit).As_Compilation_Unit.F_Body;
      Root_Node : constant Ada_Node :=
        (case Lib_Item_Or_Subunit.Kind is
           when Ada_Library_Item =>
             Lib_Item_Or_Subunit.As_Library_Item.F_Item.As_Ada_Node,
           when Ada_Subunit =>
             Lib_Item_Or_Subunit.As_Subunit.F_Body.As_Ada_Node,
         when others => raise Program_Error);

   --  Start of processing for Per_File_Action

   begin
      if Debug_Mode then
         Print (Unit);
      end if;

      case Root_Node.Kind is
         when Ada_Subp_Decl | Ada_Generic_Subp_Decl |
              Ada_Package_Decl | Ada_Generic_Package_Decl |
              Ada_Generic_Package_Instantiation =>

            --  We used to give the following "cannot have subunits" error for
            --  these as well, but now we generate the package body containing
            --  Ada stubs, and then process the body to generate subunits.

            if Root_Node.Kind in Ada_Subp_Decl | Ada_Generic_Subp_Decl
              and then Arg (Cmd, Subunits)
            then
               Cmd_Error ("argument unit cannot have subunits");
            end if;

            if not Needs_Completion (Root_Node) then
               Cmd_Error_No_Help
                 ("Compilation unit " &
                    To_UTF8 (Full_Name (Get_Def_Name (Root_Node).As_Name)) &
                    " does not require a body");
            end if;

         when Ada_Body_Node =>
            if not Arg (Cmd, Subunits) then
               Err_Out.Put ("\1: input file looks like a body\n",
                            Utils.Tool_Names.Tool_Name);
               Cmd_Error_No_Help
                 ("output file name should be provided because " &
                    File_Name &
                    " does not follow GNAT naming rules for " &
                    "spec files");
            end if;

         when others => raise Program_Error;
      end case;

      Generate_File
        (Tool, Cmd, File_Name, Input, BOM_Seen, Root_Node,
         Parent_Body_Of_Subunit => No_Ada_Node);
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Stub_Tool) is
      pragma Unreferenced (Tool);
      use Utils.Formatted_Output;
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Put ("Usage: gnatstub [options] {filename}\n");
      Put ("\n");

      Put ("  filename               Ada source file\n");
      Put ("\n");

      Put ("options:\n");
      Put ("  --version              Display version and exit\n");
      Put ("  --help                 Display usage and exit\n");
      Put ("\n");

      Put ("  -Pproject              Use project file project\n");
      Put ("  -U                     process all sources of the argument project\n");
      Put ("  -U main                process the closure of units rooted at unit main\n");
      Put ("  --no-subprojects       process sources of root project only\n");
      Put ("  -Xname=value           specify an external reference for argument project file\n");
      Put ("  -eL                    follow all symbolic links when processing project files\n");
      Put ("\n");

      Put ("  --subunits              generate separate bodies for body stubs\n");
      Put ("  --files=filename        name of a file containing a list of files to process\n");
      Put ("\n");

      Put ("  --force                replace an existing body file (if any), with a body sample\n");
      Put ("  --max-line-length=nnn  maximum line length in sample body\n");
      Put ("  --indentation=n        number of spaces used for indentation in sample body\n");
      Put ("  --alphabetical-order   alphabetically order local bodies\n");
      Put ("  --comment-header-sample insert a sample comment header\n");
      Put ("  --comment-header-spec  insert the comment header from the spec\n");
      Put ("  --header-file=filename insert the comment header from the specified file\n");
      Put ("  --no-exception         avoid raising Program_Error in procedure stubs\n");
      Put ("  --no-local-header      no local comment headers for unit stubs\n");
      Put ("  --output=body-name     the name of the file to place the body into\n");
      Put ("  --output-dir=directory place generated file(s) into directory\n");
      Put ("  --wide-character-encoding=(8|b)\n");
      Put ("                         wide character encoding of the result file\n");
      Put ("  --update-body=N        update body at line N\n");
      Put ("\n");

      Put ("  --quiet / -q           quiet mode\n");
      Put ("  --verbose / -v         verbose mode\n");

      Put ("\n\nReport bugs to report@adacore.com\n");

      pragma Style_Checks ("M79");
   end Tool_Help;

end Stub.Actions;

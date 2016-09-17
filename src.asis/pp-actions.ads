with Libadalang.Analysis; use Libadalang.Analysis;
with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Tools; use LAL_UL.Tools;

pragma Warnings (Off); -- ????
private with Ada.Containers.Hashed_Sets;
private with Langkit_Support.Vectors;
private with Langkit_Support.Slocs;
private with Libadalang.AST;
private with Libadalang.AST.Types;
private with Pp.Command_Lines;
private with LAL_UL.Generic_Symbols;
private with LAL_UL.Symbols;
pragma Warnings (On); -- ????

package Pp.Actions is

   type Pp_Tool is new Tool_State with private;

   overriding procedure Init (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Pp_Tool);

private

   use Langkit_Support;
   use Libadalang.AST;
   use Libadalang.AST.Types;
   use Pp.Command_Lines;

   --  Overall processing:????????????????
   --
   --  Init is called first. It creates a Metrix for global information (about
   --  all files).
   --
   --  Per_File_Action is called for each file. It creates a Metrix for the
   --  file, and for each relevant unit within the file. Pp are computed,
   --  but not printed. We compute all Pp, whether or not they were
   --  requested on the command line. The commmand line options control which
   --  Pp are printed.
   --
   --  Final is called. At this point, we have a tree of Metrix. The root is
   --  the all-files/global one. Children of that are per-file Metrix. Children
   --  of those are library unit and subunit Metrix. Children of those are for
   --  more-nested units. Final walks this tree and prints out all the Pp.
   --
   --  Thus, all Pp are computed before any are printed. This is necessary
   --  for coupling Pp, so it seems simplest to do it always.
   --
   --  The libadalang trees are destroyed after processing each file.
   --  Therefore, the Node component of Metrix cannot be used during printing.
   --  Any information from Node that is needed during printing must be copied
   --  into other components of Metrix. Hence the seemingly-redundant
   --  components like Kind and Sloc, below.

   type Pp_Tool is new Tool_State with record
      null;
   end record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Pp_Tool;
      Message : String := "");

   --  ????????????????Some nonsense just to get things to compile:

   pragma Style_Checks ("M200"); -- Allow long lines
   A_Block_Statement                          : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'First;
   A_Case_Path                                : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Block_Statement);
   A_Case_Statement                           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Case_Path);
   A_Comment                                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Case_Statement);
   A_Compilation_Unit                         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Comment);
   A_Component_Clause                         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Compilation_Unit);
   A_Conditional_Entry_Call_Statement         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Component_Clause);
   A_Constrained_Array_Definition             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Conditional_Entry_Call_Statement);
   A_Defining_Expanded_Name                   : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Constrained_Array_Definition);
   A_Defining_Name_List                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Defining_Expanded_Name);
   A_Derived_Record_Extension_Definition      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Defining_Name_List);
   A_Discrete_Simple_Expression_Range         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Derived_Record_Extension_Definition);
   A_Discriminant_Association                 : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Discrete_Simple_Expression_Range);
   A_For_Loop_Statement                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Discriminant_Association);
   A_Formal_Access_To_Function                : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_For_Loop_Statement);
   A_Formal_Access_To_Procedure               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Access_To_Function);
   A_Formal_Access_To_Protected_Function      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Access_To_Procedure);
   A_Formal_Access_To_Protected_Procedure     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Access_To_Protected_Function);
   A_Formal_Constrained_Array_Definition      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Access_To_Protected_Procedure);
   A_Formal_Function_Declaration              : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Constrained_Array_Definition);
   A_Formal_Object_Declaration                : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Function_Declaration);
   A_Formal_Procedure_Declaration             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Object_Declaration);
   A_Function_Body_Declaration                : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Formal_Procedure_Declaration);
   A_Function_Body_Stub                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Function_Body_Declaration);
   A_Function_Call                            : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Function_Body_Stub);
   A_Function_Declaration                     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Function_Call);
   A_Function_Renaming_Declaration            : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Function_Declaration);
   A_Generic_Association                      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Function_Renaming_Declaration);
   A_Generic_Function_Declaration             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Generic_Association);
   A_Generic_Package_Declaration              : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Generic_Function_Declaration);
   A_Generic_Procedure_Declaration            : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Generic_Package_Declaration);
   A_Loop_Statement                           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Generic_Procedure_Declaration);
   An_Accept_Statement                        : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Loop_Statement);
   An_Access_To_Function                      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Accept_Statement);
   An_Access_To_Procedure                     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Access_To_Function);
   An_Access_To_Protected_Function            : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Access_To_Procedure);
   An_Access_To_Protected_Procedure           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Access_To_Protected_Function);
   A_Named_Array_Aggregate                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Access_To_Protected_Procedure);
   An_And_Then_Short_Circuit                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Named_Array_Aggregate);
   An_Anonymous_Access_To_Function            : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_And_Then_Short_Circuit);
   An_Anonymous_Access_To_Procedure           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Anonymous_Access_To_Function);
   An_Anonymous_Access_To_Protected_Function  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Anonymous_Access_To_Procedure);
   An_Anonymous_Access_To_Protected_Procedure : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Anonymous_Access_To_Protected_Function);
   An_Array_Component_Association             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Anonymous_Access_To_Protected_Procedure);
   An_Aspect_Specification                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Array_Component_Association);
   An_Asynchronous_Select_Statement           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Aspect_Specification);
   An_Else_Path                               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Asynchronous_Select_Statement);
   An_Elsif_Path                              : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Else_Path);
   An_Entry_Body_Declaration                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Elsif_Path);
   An_Entry_Call_Statement                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Entry_Body_Declaration);
   An_Entry_Declaration                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Entry_Call_Statement);
   An_Enumeration_Representation_Clause       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Entry_Declaration);
   An_Enumeration_Type_Definition             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Enumeration_Representation_Clause);
   An_Exponentiate_Operator                   : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Enumeration_Type_Definition);
   An_Expression_Function_Declaration         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Exponentiate_Operator);
   An_Extended_Return_Statement               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Expression_Function_Declaration);
   An_Extension_Aggregate                     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Extended_Return_Statement);
   An_Identifier                              : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Extension_Aggregate);
   An_If_Path                                 : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Identifier);
   An_Implementation_Defined_Attribute        : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_If_Path);
   An_Index_Constraint                        : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Implementation_Defined_Attribute);
   An_In_Mode                                 : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Index_Constraint);
   An_In_Out_Mode                             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_In_Mode);
   An_Integer_Literal                         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_In_Out_Mode);
   An_Is_Prefix_Call                          : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Integer_Literal);
   An_Is_Prefix_Notation                      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Is_Prefix_Call);
   An_Ordinary_Type_Declaration               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Is_Prefix_Notation);
   An_Or_Else_Short_Circuit                   : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Ordinary_Type_Declaration);
   An_Out_Mode                                : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Or_Else_Short_Circuit);
   A_Null_Procedure_Declaration               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (An_Out_Mode);
   A_Null_Statement                           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Null_Procedure_Declaration);
   A_Package_Body_Declaration                 : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Null_Statement);
   A_Package_Declaration                      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Package_Body_Declaration);
   A_Parameter_Association                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Package_Declaration);
   A_Parameter_Specification                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Parameter_Association);
   A_Positional_Array_Aggregate               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Parameter_Specification);
   A_Pragma_Argument_Association              : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Positional_Array_Aggregate);
   A_Procedure_Body_Declaration               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Pragma_Argument_Association);
   A_Procedure_Body_Stub                      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Procedure_Body_Declaration);
   A_Procedure_Call_Statement                 : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Procedure_Body_Stub);
   A_Procedure_Declaration                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Procedure_Call_Statement);
   A_Procedure_Renaming_Declaration           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Procedure_Declaration);
   A_Protected_Body_Declaration               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Procedure_Renaming_Declaration);
   A_Protected_Type_Declaration               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Protected_Body_Declaration);
   A_Qualified_Expression                     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Protected_Type_Declaration);
   A_Range_Attribute_Reference                : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Qualified_Expression);
   A_Real_Literal                             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Range_Attribute_Reference);
   A_Record_Aggregate                         : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Real_Literal);
   A_Record_Component_Association             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Record_Aggregate);
   A_Record_Representation_Clause             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Record_Component_Association);
   A_Record_Type_Definition                   : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Record_Representation_Clause);
   A_Selected_Component                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Record_Type_Definition);
   A_Selective_Accept_Statement               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Selected_Component);
   A_Simple_Expression_Range                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Selective_Accept_Statement);
   A_Single_Protected_Declaration             : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Simple_Expression_Range);
   A_Single_Task_Declaration                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Single_Protected_Declaration);
   A_String_Literal                           : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Single_Task_Declaration);
   A_Subtype_Indication                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_String_Literal);
   A_Subunit                                  : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Subtype_Indication);
   A_Tagged_Record_Type_Definition            : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Subunit);
   A_Task_Body_Declaration                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Tagged_Record_Type_Definition);
   A_Task_Type_Declaration                    : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Task_Body_Declaration);
   A_Timed_Entry_Call_Statement               : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Task_Type_Declaration);
   A_Unary_Minus_Operator                     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Timed_Entry_Call_Statement);
   A_Unary_Plus_Operator                      : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Unary_Minus_Operator);
   A_Use_Package_Clause                       : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Unary_Plus_Operator);
   A_While_Loop_Statement                     : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_Use_Package_Clause);
   A_With_Clause                              : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Succ (A_While_Loop_Statement);

end Pp.Actions;

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

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools;         use Utils.Tools;

private with Ada.Containers.Hashed_Sets;
private with Utils.Vectors;
private with Langkit_Support.Slocs;
private with METRICS.Command_Lines;
private with METRICS.Line_Counting;
private with Utils.Generic_Symbols;
private with Utils.Symbols;

package METRICS.Actions is

   type Metrics_Tool is new Tool_State with private;

private

   overriding procedure Init
     (Tool : in out Metrics_Tool; Cmd : in out Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Metrics_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Metrics_Tool);

   use Langkit_Support;
   use type Slocs.Source_Location_Range;
   use METRICS.Command_Lines;
   use METRICS.Line_Counting;

   use Utils.Symbols;
   subtype Symbol is Utils.Symbols.Symbol;

   package CU_Symbols is new Utils.Generic_Symbols;
   use CU_Symbols;
   subtype CU_Symbol is CU_Symbols.Symbol;
   --  The name of a compilation unit

   Empty_CU_Sym : constant CU_Symbol := Intern ("");

   package CU_Symbol_Sets is new Ada.Containers.Hashed_Sets
     (CU_Symbol, Hash_Symbol, Equivalent_Elements => Case_Insensitive_Equal);
   use CU_Symbol_Sets;

   type Metrics_Values is array (Metrics_Enum) of Metric_Nat;
   Initial_Metrics_Values : constant Metrics_Values :=
     [Complexity_Statement |
        Complexity_Cyclomatic |
        Complexity_Essential |
        Contract_Complexity => 1,
      others => 0];

   type Metrix;
   type Metrix_Ref is access all Metrix;

   type Metrix_Index is new Positive;
   type Metrix_Array is array (Metrix_Index range <>) of Metrix_Ref;
   package Metrix_Vectors is
      new Utils.Vectors (Metrix_Index, Metrix_Ref, Metrix_Array);
   use Metrix_Vectors;

   Null_Kind : constant Ada_Node_Kind_Type := Ada_Abort_Absent;
   --  ???We need a special value

   type Fine_Kind is
   --  This is an enumeration of all the node kinds that we collect metrics
   --  for. It is "finer" than Ada_Node_Kind_Type in the sense that procedures
   --  and functions get their own kinds (instead of being lumped together as
   --  subprograms). The names of these are chosen so that the 'Image can be
   --  used to compute the string to be printed (e.g., Generic_Package_Knd
   --  prints as "generic package").
     (No_Such_Knd,
      Generic_Package_Knd,
      Package_Body_Knd,
      Package_Knd,
      Protected_Body_Knd,
      Protected_Object_Knd,
      Protected_Type_Knd,
      Entry_Body_Knd,
      Procedure_Body_Knd,
      Function_Body_Knd,
      Procedure_Body_Stub_Knd,
      Function_Body_Stub_Knd,
      Task_Body_Knd,
      Task_Object_Knd,
      Task_Type_Knd,
      Function_Instantiation_Knd,
      Package_Instantiation_Knd,
      Procedure_Instantiation_Knd,
      Generic_Package_Renaming_Knd,
      Generic_Procedure_Renaming_Knd,
      Generic_Function_Renaming_Knd,
      Generic_Procedure_Knd,
      Generic_Function_Knd,
      Package_Renaming_Knd,
      Procedure_Knd,
      Function_Knd,
      Null_Procedure_Knd,
      Expression_Function_Knd);

   --  Overall processing:
   --
   --  Init is called first. It creates a Metrix for global information (about
   --  all files).
   --
   --  Per_File_Action is called for each file. It creates a Metrix for the
   --  file, and for each relevant unit within the file. Metrics are computed,
   --  but not printed. We compute all metrics, whether or not they were
   --  requested on the command line. The commmand line options control which
   --  metrics are printed.
   --
   --  Final is called. At this point, we have a tree of Metrix. The root is
   --  the all-files/global one. Children of that are per-file Metrix. Children
   --  of those are library unit and subunit Metrix. Children of those are for
   --  more-nested units. Final walks this tree and prints out all the metrics.
   --
   --  Thus, all metrics are computed before any are printed. This is necessary
   --  for coupling metrics, so it seems simplest to do it always.
   --
   --  The libadalang trees are destroyed after processing each file.
   --  Therefore, the Node component of Metrix cannot be used during printing.
   --  Any information from Node that is needed during printing must be copied
   --  into other components of Metrix. Hence the seemingly-redundant
   --  components like Kind and Sloc, below.

   type Metrix (Kind : Ada_Node_Kind_Type) is limited record
      Node : Ada_Node := No_Ada_Node;
      --  Node to which the metrics are associated, except for Metrix_Stack[1],
      --  which has Node = null. Node is used only while gathering metrics; it
      --  is not used while printing metrics.

      --  The Kind discriminant is equal to Node.Kind, or Null_Kind for
      --  Metrix_Stack[1].

      Knd : Fine_Kind;
      --  Finer-grained version of Kind

      Sloc : Slocs.Source_Location_Range;
      --  Equal to the Sloc of Node

      Is_Private_Lib_Unit : Boolean;
      --  True if this is a private library unit

      Visible : Boolean := False;
      --  True if the node is public as defined by gnatmetric -- not nested in
      --  any body or private part. Used for Contract_Complexity, which should
      --  be displayed only for public subprograms. (The other contract metrics
      --  are also displayed only for public subprograms, but they use a
      --  different mechanism.)

      Has_Complexity_Metrics : Boolean := False;
      --  True if complexity metrix should be computed for Node (assuming it's
      --  requested on the command line).

      Text_Name : Symbol;
      --  Name of the unit, as printed in text output
      XML_Name : Symbol;
      --  Name of the unit, as printed in XML output
      LI_Sub : Symbol;
      --  For the outermost unit, this is a string indicating whether the unit
      --  is a subunit or a library unit. For other units, this is the empty
      --  string.
      --  Above symbols are undefined for Metrix_Stack[1].

      Comp_Unit : Metrix_Ref := null;
      --  The compilation unit in which this Metrix is nested.
      --  Null for Metrix_Stack[1]. For Metrix_Stack[2], points
      --  to itself.

      Vals : Metrics_Values := Initial_Metrics_Values;

      Submetrix : Metrix_Vectors.Vector;
      --  Metrix records for units nested within this one

      case Kind is
         when Ada_Compilation_Unit | Null_Kind =>
            Num_With_Complexity : Metric_Nat := 0;
            --  Number of descendants for which complexity metrics apply

            case Kind is
               when Ada_Compilation_Unit =>
                  Is_Spec : Boolean;

                  CU_Name : CU_Symbol;
                  --  Name of this compilation unit

                  Subunit_Parent : CU_Symbol;
                  --  If this is a subunit, name of the parent; empty string
                  --  otherwise.

                  Child_Parent : CU_Symbol := Empty_CU_Sym;
                  --  If this is a child unit, name of the parent; empty string
                  --  otherwise. Do we really need separate _Parent fields???
                  --  Perhaps we should compute these together, in
                  --  Push_New_Metrix.

                  Depends_On : CU_Symbol_Sets.Set;
                  Indirect_Dependences_Computed : Boolean := False;
                  --  Depends_On is the set of compilation units this one
                  --  depends upon. It is computed in 3 steps:
                  --
                  --     During the initial walk of each source file tree,
                  --     Gather_Dependencies sets it to include just direct
                  --     dependencies.
                  --
                  --     During Final, Compute_Indirect_Dependencies first
                  --     removes units that do not exist in the set of units
                  --     being processed. It then walks the dependence graph,
                  --     and computes indirect dependences.
                  --     Indirect_Dependences_Computed is set to True as each
                  --     node in the graph is processed.

                  --     Finally Compute_Coupling, merges the Depends_On from
                  --     bodies into the corresponding library unit spec,
                  --     because that's what coupling metrics want.
                  --
                  --  This is used for coupling metrics, so it doesn't exactly
                  --  match the Ada notion of dependence. As mentioned above,
                  --  body dependences are merged with the spec. Body-->spec
                  --  and subunit-->parent-body dependences are not recorded.
                  --  This is because coupling metrics treat a spec along with
                  --  its body and subunits as a single entity.
                  --
                  --  We're working with names here, because this was written
                  --  before libadalang supported semantic information. This
                  --  works because library unit names are unique. We use a set
                  --  so that redundancies don't count (e.g. "with X; with X;"
                  --  should count as depending on X (once)).

                  Has_Tagged_Type, Has_Subp : Boolean := False;
                  --  True if this is a unit containing a tagged type or a
                  --  subprogram. Used to compute coupling metrics. Ignored for
                  --  bodies.

                  Source_File_Name : String_Ref := null;

               when others =>
                  null;
            end case;

         when Ada_Package_Body =>
            Statements_Sloc : Slocs.Source_Location_Range :=
              Slocs.No_Source_Location_Range;
            --  For a package body with statements, this is their location.
            --  No_Source_Location_Range if there are no statements.

         when Ada_Subp_Body | Ada_Subp_Body_Stub =>
            Acts_As_Spec : Boolean;
            --  True for a subprogram body with no corresponding spec

         when others =>
            null;
      end case;
   end record; -- Metrix

   type Metrics_Tool is new Tool_State with record
      Metrics_To_Compute : Metrics_Set;
      --  Metrics requested via command line args

      Metrix_Stack : Metrix_Vectors.Vector;
      --  Metrix_Stack[1] is the global Metrix (totals for all files).
      --
      --  Metrix_Stack[2] is the Metrix for the Compilation_Unit node.
      --  This is for per-file metrics. Note that lalmetric does not
      --  yet support multiple compilation units per file.
      --
      --  Metrix_Stack[3] is the Metrix for the library item within that; this
      --  is a Package_Decl, Package_Body, or whatever node.
      --
      --  The rest are Metrix for the nested nodes that are "eligible" for
      --  computing metrics. See subtype Eligible in the body.
      --
      --  This stack contains the relevant nodes currently being processed by
      --  the recursive walk. It is used while computing the metrics; printing
      --  the metrics walks the tree formed by Submetrix.

      Treat_Exit_As_Goto : Boolean; -- Option for Complexity_Essential
   end record;

   --  Init is called once, before processing any files. It pushes
   --  Metrix_Stack[1].
   --
   --  Then for each file, we walk the tree, pushing and popping the
   --  Metrix_Stack as we go. When we push a Metrix, we append it to
   --  the Submetrix of its parent, so when we're done walking the
   --  tree, the Metrix form a tree as well.
   --
   --  At each node, we increment relevant Vals, depending on the kind
   --  of node. For example, if we see a node that is a statement, we
   --  increment all the Vals(Statements) of all the Metrix in Metrix
   --  stack. Thus Vals(Statements) for a unit will include the number
   --  of statement in nested units, and Metrix_Stack[1].Vals(Statements)
   --  will contain to total number of statements in all files.
   --
   --  Final is called once, after processing all files. It prints out
   --  the totals for all files that have been computed in
   --  Metrix_Stack[1].
   --
   --  We always compute all metrics. The metrics requested on the
   --  command line are taken into account when we print the data.

   --  For Debugging:

   procedure Dump_Metrix (M : Metrix);
   procedure Dump
     (Tool : in out Metrics_Tool;
      Global_M : Metrix;
      Message : String := "");

end METRICS.Actions;

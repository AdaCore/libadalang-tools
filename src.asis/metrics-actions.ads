with Text_IO;

with Langkit_Support.Vectors;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.AST; use Libadalang.AST;

with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Tools; use LAL_UL.Tools;

with METRICS.Command_Lines; use METRICS.Command_Lines;

package METRICS.Actions is

   type Metrics_Tool is new Tool_State with private;

   overriding procedure Init (Tool : in out Metrics_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Metrics_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Metrics_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Metrics_Tool);

private

   subtype Metric_Int is Natural;

   type Metrics_Values is array (Metrics_Enum) of Metric_Int;

   type Metrix;
   type Metrix_Ref is access all Metrix;

   package Metrix_Vectors is new Langkit_Support.Vectors (Metrix_Ref);
   use Metrix_Vectors;

   type Metrix is record
      Node : Ada_Node := null;
      --  Node to which the metrics are associated, except for
      --  Metrix_Stack[0], which has Node = null.

      Visible : Boolean := False;
      --  True if the node is public as defined by gnatmetric -- not nested in
      --  any body or private part. Used for Contract_Complexity, which should
      --  be displayed only for public subprograms. (The other contract metrics
      --  are also displayed only for public subprograms, but they use a
      --  different mechanism.)

      Vals : Metrics_Values :=
        (Complexity_Statement |
         Complexity_Cyclomatic |
         Complexity_Essential |
         Contract_Complexity => 1,
         others => 0);

      Num_With_Complexity : Metric_Int := 0;
      --  Number of descendants for which complexity metrics apply. Used
      --  only for the Compilation_Unit, which is used to represent the
      --  per-file metrics.

      Submetrix : Metrix_Vectors.Vector;
      --  Metrix records for units nested within this one
   end record;

   type Metrics_Tool is new Tool_State with record
      XML : Text_IO.File_Type;
      --  All XML output for all files goes to this file.

      Metrics_To_Compute : Metrics_Set;
      --  Metrics requested on via command line args

      Metrix_Stack : Metrix_Vectors.Vector;
      --  Metrix_Stack[0] is the global Metrix (totals for all files).
      --
      --  Metrix_Stack[1] is the Metrix for the Compilation_Unit node.
      --  This is for per-file metrics.
      --
      --  Metrix_Stack[2] is the Metrix for the library item within that; this
      --  is a Package_Decl, Package_Body, or whatever node.
      --
      --  The rest are Metrix for the nested nodes that are "eligible" for
      --  computing metrics. These nodes are [generic] package specs, single
      --  task/protected declarations, task/protected type declarations, and
      --  proper bodies other than entry bodies.
      --
      --  This stack contains the relevant nodes currently being processed by
      --  the recursive walk.
   end record;

   --  Init is called once, before processing any files. It pushes
   --  Metrix_Stack[0].
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
   --  of statement in nested units, and Metrix_Stack[0].Vals(Statements)
   --  will contain to total number of statements in all files.
   --
   --  Final is called once, after processing all files. It prints out
   --  the totals for all files that have been computed in
   --  Metrix_Stack[0].
   --
   --  We always compute all metrics. The metrics requested on the
   --  command line are taken into account when we print the data.

end METRICS.Actions;

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
      Node : Ada_Node;
      --  Node to which the metrics are associated

      Vals : Metrics_Values;

      Submetrix : Metrix_Vectors.Vector;
      --  Metrix records for units nested within this one
   end record;

   type Metrics_Tool is new Tool_State with record
      XML : Text_IO.File_Type;
      --  All XML output for all files goes to this file.

      Metrics_To_Compute : Metrics_Set;
      --  Metrics requested on via command line args

      Metrix_Stack : Metrix_Vectors.Vector;
      --  ????????????????Actually, we have one for global metrics.
      --  Metrix_Stack[0] is the Metrix for the Compilation_Unit node.
      --
      --  Metrix_Stack[1] is the Metrix for the library item within that; this
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

end METRICS.Actions;

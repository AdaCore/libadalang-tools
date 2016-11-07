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

end Pp.Actions;

with Libadalang.Analysis; use Libadalang.Analysis;
with ASIS_UL.Char_Vectors; use ASIS_UL.Char_Vectors;
use ASIS_UL.Char_Vectors.Char_Vectors;
with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Tools; use LAL_UL.Tools;

package Pp.Actions is

   type Pp_Tool is new Tool_State with private;

   procedure Format_Vector
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : Char_Vector;
      Output : out Char_Vector;
      Unit : Analysis_Unit);
   --  This pretty prints the given source. The input source text is in Source,
   --  and the output source text is left in Source. Note that the gnatpp
   --  program does not call this directly; it calls Per_File_Action.
   --  Format_Vector is for calling from text editors and the like.
   --  File_Name is for printing error messages; it can be empty if there
   --  is none. Modified indicates whether Source was modified.

private

   overriding procedure Init (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Pp_Tool);

   type Pp_Tool is new Tool_State with null record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Pp_Tool;
      Message : String := "");

end Pp.Actions;

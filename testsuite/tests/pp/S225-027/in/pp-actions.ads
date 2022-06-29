with Libadalang.Analysis; use Libadalang.Analysis;
with Utils.Char_Vectors;  use Utils.Char_Vectors;
use Utils.Char_Vectors.Char_Vectors;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools;         use Utils.Tools;
with Pp.Scanner;

package Pp.Actions is

   type Pp_Tool is new Tool_State with private;

   procedure Format_Vector
     (Cmd       :     Command_Line; Input : Char_Vector; Node : Ada_Node;
      In_Range  :     Char_Subrange; Output : out Char_Vector;
      Out_Range : out Char_Subrange;
      Messages  : out Pp.Scanner.Source_Message_Vector) with
     Pre => Pp.Scanner.Source_Message_Vectors.Is_Empty (Messages);
--  This pretty prints the given source. Parameters:
   --
   --     Cmd -- processed command line, or "command line" concocted by a tool
   --     such as GPS.
   --
   --     Input -- input source text. Can be empty if we are not processing an
   --     entire file. When called from GPS, this should be the entire file
   --     contents, even if In_Range indicates a smaller region to format.
   --
   --     Node -- Root of the Ada tree to format.
   --
   --     In_Range -- Range of text in Input that is relevant to the client.
   --
   --     Output -- formatted text. When called from GPS, this will be the
   --     entire whole-file result; it can use Out_Range to snip out the
   --     part that should be formatted.
   --
   --     Out_Range -- Range of text in Output that corresponds to In_Range.
   --
   --     Messages -- Error messages.
   --
   --  If Messages is not empty, then the client should notify the user
   --  somehow, and avoid updating the user's source code. In addition, if
   --  Format_Vector raises an exception, that is a bug, and the client
   --  should avoid updating the user's source code.
   --
   --  If the portion of Output indicated by Out_Range is identical to the
   --  portion of Input indicated by In_Range, then clients should normally
   --  avoid changing the user's source code.
   --
   --  Note that the gnatpp program does not call this directly; it calls
   --  Per_File_Action. Format_Vector is for calling from text editors and
   --  the like. Format_Vector is called from lalstub.

private

   overriding procedure Init
     (Tool : in out Pp_Tool; Cmd : in out Command_Line);
   overriding procedure Per_File_Action
     (Tool  : in out Pp_Tool; Cmd : Command_Line; File_Name : String;
      Input :        String; BOM_Seen : Boolean; Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Pp_Tool);

   type Pp_Tool is new Tool_State with null record;

   --  For Debugging:

   procedure Dump (Tool : in out Pp_Tool; Message : String := "");

end Pp.Actions;

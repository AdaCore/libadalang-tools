--  pack.ads

package Pack is
   procedure Proc (I : Integer; B : Boolean);

   package Inner is
      procedure P1 (I : Integer);
      function F1 (I : Integer) return Boolean;
   end Inner;

end Pack;

----------------------------------------------------------------

--  pack.adb

package body Pack is
   package body Inner is separate;
end Pack;

----------------------------------------------------------------

--  pack-child.ads

package Pack.Child is
   procedure Proc;

   protected type P_Rec is
      entry E (I : in out Integer);
      procedure P (B : Boolean; I : out Integer);
      function F return Integer;
   private
      State : Integer;
   end;

   procedure A;
   procedure B;

end Pack.Child;

----------------------------------------------------------------

--  pack-child.adb

package body Pack.Child is
end Pack.Child;

----------------------------------------------------------------

--  mumble.ads

package Mumble is

end Mumble;

----------------------------------------------------------------

--  mumble-processing.ads

with Libadalang.Analysis; use Libadalang.Analysis;
with Utils.Char_Vectors; use Utils.Char_Vectors;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools; use Utils.Tools;
with Pp.Scanner; use Pp;

package Mumble.Processing is

   type Stub_Tool is new Tool_State with private;

private

   overriding procedure Init (Tool : in out Stub_Tool; Cmd : Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Stub_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Stub_Tool);

   type Stub_Tool is new Tool_State with record
      Ignored_Out_Range : Char_Subrange;
      Ignored_Messages : Scanner.Source_Message_Vector;
   end record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Stub_Tool;
      Message : String := "");

end Mumble.Processing;

----------------------------------------------------------------

--  mumble-processing.adb

package body Mumble.Processing is

   procedure Init (Tool : in out Stub_Tool; Cmd : Command_Line) is
   begin
      null;
   end Init;

   procedure Tool_Help (Tool : Stub_Tool) is
   begin
      null;
   end Tool_Help;

end Mumble.Processing;

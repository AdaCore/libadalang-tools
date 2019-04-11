with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;

with GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Utils.Tool_Names is

   Exe_Suffix : constant String_Access := Get_Executable_Suffix;
   Tool_Name  : constant String        :=
     To_Lower
       (GNAT.Directory_Operations.Base_Name
          (Ada.Command_Line.Command_Name,
           Suffix => Exe_Suffix.all));

   function Target return String;
   --  If this is a cross version of the tool, Tool_Name will be of the form
   --  target-tool, and this returns "target". If the tool name starts with
   --  "gnaamp", returns "AAMP". Otherwise, this returns "".

   function Basic_Tool_Name return String;
   --  Returns the tool name without the target & "-", if any

   Full_Tool_Name : String renames
     Locate_Exec_On_Path (Ada.Command_Line.Command_Name).all;
   --  Full path name of the tool executable file. Cannot be null.

end Utils.Tool_Names;

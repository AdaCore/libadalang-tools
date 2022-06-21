------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Interfaces;
with Langkit_Support.Slocs;
use Langkit_Support.Slocs;
with Langkit_Support.Text;
with VSS.Strings;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions;
with VSS.Strings.Conversions;
use VSS.Strings.Conversions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Laltools.Refactor; use Laltools.Refactor;
with VSS.JSON.Content_Handlers;
with VSS.JSON.Push_Writers;

package body Output is
   package Text renames Langkit_Support.Text;
   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item      : Source_Location;
      File_Name : String;
      Success   : in out Boolean);
   --  Write the location information using source_location with
   --  column excluded.

   procedure Write_Include
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item      : Source_Location;
      File_Name : String;
      Success   : in out Boolean);
   --  Write the location information using source_location with
   --  column included.

   procedure Write_Node
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : LAL.Ada_Node'Class;
      Success : in out Boolean);
   --  Write the record location information

   procedure Write_Warning
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Success : in out Boolean);
   --  Write the warning information

   procedure Write_Fixit
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Text_Edit_Map;
      Success : in out Boolean);
   --  Write the fixits information for Text_Edit_Maps

   procedure Write_Fixit
     (Writer   : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Filename : String;
      Item     : Text_Edit_Ordered_Set;
      Success  : in out Boolean);
   --  Write the fixits information for Text_Edit_Ordered_Sets

   procedure Write_Edit
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      File_Name : String;
      Item      : Text_Edit;
      Success   : in out Boolean);
   --  Write the delete locations

   procedure Write_Message
     (Writer       : in out VSS.JSON.Content_Handlers.
                     JSON_Content_Handler'Class;
      Record_Name  : LAL.Defining_Name'Class;
      Delete_Names : Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean);
   --  Write the error/warning messages.
   --  Attention this one only works for the record_componenet_tool

   procedure Write_Message
     (Writer         : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Aggregate_Node : LAL.Aggregate'Class;
      Success        : in out Boolean);
   --  Write the error/warning messages.
   --  For Array_Aggregate

   procedure Write_Message
     (Writer       : in out VSS.JSON.Content_Handlers.
                     JSON_Content_Handler'Class;
      Func_Name    : LAL.Defining_Name'Class;
      Delete_Names : Tools.Suppress_Dead_Params_Tool.
                     Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean);
   --  Write the error/warning messages.
   --  For Suppress_Params

   procedure Write_Message
     (Writer       : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl     : LAL.Object_Decl;
      Modify_Names : Tools.Scope_Declarations_Tool
                     .Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean);
   --  Write the error/warning messages.
   --  For Scope_Declarations

   procedure Write
     (Writer       : in out VSS.JSON.Content_Handlers.
                     JSON_Content_Handler'Class;
      Record_Name  : LAL.Defining_Name'Class;
      Delete_Names : Defining_Name_Ordered_Sets.Set;
      Item         : Text_Edit_Map;
      Success      : in out Boolean);
   --  Write for record_component

   procedure Write
     (Writer         : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Aggregate_Node : LAL.Aggregate;
      Item           : Text_Edit_Ordered_Set;
      Success        : in out Boolean);
   --  Write for array_aggregate

   procedure Write
     (Writer        : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Function_Name : LAL.Defining_Name;
      Item          : Text_Edit_Map;
      Params        : Tools.Suppress_Dead_Params_Tool
                      .Defining_Name_Ordered_Sets.Set;
      Success       : in out Boolean);
   --  Write for suppress_params

   procedure Write
     (Writer        : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl_Name : LAL.Object_Decl;
      Item          : Text_Edit_Map;
      Names         : Tools.Scope_Declarations_Tool
                      .Defining_Name_Ordered_Sets.Set;
      Success       : in out Boolean);
   --  Write for scope_declarations

   procedure Write
     (Writer        : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl_Name : LAL.Object_Decl;
      Item          : Text_Edit_Map;
      Names         : Tools.Relocate_Decls_Tool
                      .Defining_Name_Ordered_Sets.Set;
      Success       : in out Boolean);
   --  Write for relocate_decls object decl part

   procedure Write_Message_Private
     (Writer       : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl     : LAL.Object_Decl;
      Modify_Names : Tools.Relocate_Decls_Tool
                     .Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean);
   --  Create message for the relocate_decls for the obj decls part

   procedure Write_Message_Private
     (Writer    : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Decl_Name : LAL.Defining_Name;
      Success   : in out Boolean);
   --  Create message for the relocate_decls for the other decls part

   procedure Write_Removable_Decl_Part
     (Writer  : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Success : in out Boolean);
   --  Create message that suggests remove the whole declarative part

   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.
                  JSON_Content_Handler'Class;
      Decl_Part : LAL.Declarative_Part;
      Item      : Text_Edit_Map;
      Success   : in out Boolean);

   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.
                  JSON_Content_Handler'Class;
      Decl_Name : LAL.Defining_Name;
      Item      : Text_Edit_Map;
      Success   : in out Boolean);
   --  Write for relocate_decls other decls part

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item      : Source_Location;
      File_Name : String;
      Success   : in out Boolean)
   is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("column", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Item.Column), Success);

      Writer.Key_Name ("file", Success);
      Writer.String_Value (VSS.Strings.To_Virtual_String
                           (To_Wide_Wide_String (File_Name)),
                           Success);

      Writer.Key_Name ("line", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Item.Line), Success);

      Writer.End_Object (Success);
   end Write;

   -------------------
   -- Write_Include --
   -------------------

   procedure Write_Include
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item      : Source_Location;
      File_Name : String;
      Success   : in out Boolean)
   is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("column", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Item.Column - 1), Success);

      Writer.Key_Name ("file", Success);
      Writer.String_Value (VSS.Strings.To_Virtual_String
                           (To_Wide_Wide_String (File_Name)),
                           Success);

      Writer.Key_Name ("line", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Item.Line), Success);

      Writer.End_Object (Success);
   end Write_Include;

   -----------------
   -- Write_Fixit --
   -----------------

   procedure Write_Fixit
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item    : Text_Edit_Map;
      Success : in out Boolean)
   is
   begin
      Writer.Key_Name ("fixits", Success);
      Writer.Start_Array (Success);
      for File_Name in Item.Iterate loop

         for Text_To_Edit of Item (File_Name) loop
            Write_Edit (Writer,
                        File_Name.Key,
                        Text_To_Edit,
                        Success);
         end loop;
      end loop;

      Writer.End_Array (Success);
   end Write_Fixit;

   -----------------
   -- Write_Fixit --
   -----------------

   procedure Write_Fixit
     (Writer   : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Filename : String;
      Item     : Text_Edit_Ordered_Set;
      Success  : in out Boolean)
   is
   begin
      Writer.Key_Name ("fixits", Success);
      Writer.Start_Array (Success);
      for Text_To_Edit of Item loop
         Write_Edit (Writer,
                     Filename,
                     Text_To_Edit,
                     Success);
      end loop;
      Writer.End_Array (Success);
   end Write_Fixit;

   ------------------
   -- Write_Delete --
   ------------------

   procedure Write_Edit
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      File_Name : String;
      Item      : Text_Edit;
      Success   : in out Boolean)
   is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("next", Success);
      Write (Writer, End_Sloc (Item.Location), File_Name, Success);

      Writer.Key_Name ("start", Success);
      Write (Writer, Start_Sloc (Item.Location), File_Name, Success);

      Writer.Key_Name ("string", Success);
      Writer.String_Value (VSS.Strings.Conversions
                           .To_Virtual_String (Item.Text),
                           Success);

      Writer.End_Object (Success);
   end Write_Edit;

   ----------------
   -- Write_Node --
   ----------------

   procedure Write_Node
     (Writer         : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Item           : LAL.Ada_Node'Class;
      Success        : in out Boolean) is
      Location_Range : constant Source_Location_Range := LAL.Sloc_Range (Item);
      File_Name      : constant String := Item.Unit.Get_Filename;
   begin
      Writer.Start_Array (Success);
      Writer.Start_Object (Success);

      Writer.Key_Name ("caret", Success);
      Write (Writer, Start_Sloc (Location_Range), File_Name, Success);

      Writer.Key_Name ("finish", Success);
      Write_Include (Writer, End_Sloc (Location_Range), File_Name, Success);

      Writer.End_Object (Success);
      Writer.End_Array (Success);
   end Write_Node;

   -------------------
   -- Write_Warning --
   -------------------

   procedure Write_Warning
     (Writer  : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Success : in out Boolean)
   is
   begin
      Writer.Key_Name ("kind", Success);
      Writer.String_Value ("warning", Success);
   end Write_Warning;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Writer       : in out VSS.JSON.Content_Handlers.
                     JSON_Content_Handler'Class;
      Record_Name  : LAL.Defining_Name'Class;
      Delete_Names : Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean)
   is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The component(s) ";
      for Name of Delete_Names loop
         Words := Words & "'" & Text.Image (Name.Text) & "' ";
      end loop;
      Words := Words & "in the record '"
               & Text.Image (Record_Name.Text) & "' can be remove";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Writer       : in out VSS.JSON.Content_Handlers.
                     JSON_Content_Handler'Class;
      Func_Name    : LAL.Defining_Name'Class;
      Delete_Names : Tools.Suppress_Dead_Params_Tool.
                     Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean)
   is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The parameter(s) ";
      for Name of Delete_Names loop
         Words := Words & "'" & Text.Image (Name.Text) & "' ";
      end loop;
      Words := Words & "in the subprogram '"
               & Text.Image (Func_Name.Text) & "' are always using same value";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Writer         : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Aggregate_Node : LAL.Aggregate'Class;
      Success        : in out Boolean)
   is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The Aggregate ";
      Words := Words & Text.Image (Aggregate_Node.Text) &
        " Array aggregate using () is an obsolescent syntax, use [] instead.";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Writer       : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl     : LAL.Object_Decl;
      Modify_Names : Tools.Scope_Declarations_Tool
                     .Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean) is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The variable(s) ";
      for Name of Modify_Names loop
         Words := Words & "'" & Text.Image (Name.Text) & "' ";
      end loop;
      Words := Words & "in the declaration '"
        & Text.Image (Obj_Decl.Text) &
        "' can be scoped more narrowly";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message;

   procedure Write_Removable_Decl_Part
     (Writer  : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Success : in out Boolean)
   is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "This declarative part could possibly be empty, can be"
               & " removed entirely";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Removable_Decl_Part;

   ---------------------------
   -- Write_Message_Private --
   ---------------------------

   procedure Write_Message_Private
     (Writer       : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl     : LAL.Object_Decl;
      Modify_Names : Tools.Relocate_Decls_Tool
                     .Defining_Name_Ordered_Sets.Set;
      Success      : in out Boolean) is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The variable(s) ";
      for Name of Modify_Names loop
         Words := Words & "'" & Text.Image (Name.Text) & "' ";
      end loop;
      Words := Words & "in the declaration '"
        & Text.Image (Obj_Decl.Text) &
        "' can be relocated to better place";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message_Private;

   ---------------------------
   -- Write_Message_Private --
   ---------------------------

   procedure Write_Message_Private
     (Writer    : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Decl_Name : LAL.Defining_Name;
      Success   : in out Boolean) is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The Declaration of '" & Text.Image (Decl_Name.Text)
               & "' can be relocated to better place";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message_Private;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer       : in out VSS.JSON.Content_Handlers.
                     JSON_Content_Handler'Class;
      Record_Name  : LAL.Defining_Name'Class;
      Delete_Names : Defining_Name_Ordered_Sets.Set;
      Item         : Text_Edit_Map;
      Success      : in out Boolean)
   is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Record_Name.As_Ada_Node, Success);

      Write_Message (Writer, Record_Name, Delete_Names, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer         : in out VSS.JSON.Content_Handlers.
                       JSON_Content_Handler'Class;
      Aggregate_Node : LAL.Aggregate;
      Item           : Text_Edit_Ordered_Set;
      Success        : in out Boolean)
   is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Aggregate_Node.Unit.Get_Filename, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Aggregate_Node.As_Ada_Node, Success);

      Write_Message (Writer, Aggregate_Node, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer        : in out VSS.JSON.Content_Handlers.
                      JSON_Content_Handler'Class;
      Function_Name : LAL.Defining_Name;
      Item          : Text_Edit_Map;
      Params        : Tools.Suppress_Dead_Params_Tool
                      .Defining_Name_Ordered_Sets.Set;
      Success       : in out Boolean)
   is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Function_Name.As_Ada_Node, Success);

      Write_Message (Writer, Function_Name, Params, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer        : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl_Name : LAL.Object_Decl;
      Item          : Text_Edit_Map;
      Names         : Tools.Scope_Declarations_Tool
                      .Defining_Name_Ordered_Sets.Set;
      Success       : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Obj_Decl_Name.As_Ada_Node, Success);

      Write_Message (Writer, Obj_Decl_Name, Names, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer        : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Obj_Decl_Name : LAL.Object_Decl;
      Item          : Text_Edit_Map;
      Names         : Tools.Relocate_Decls_Tool
                      .Defining_Name_Ordered_Sets.Set;
      Success       : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Obj_Decl_Name.As_Ada_Node, Success);

      Write_Message_Private (Writer, Obj_Decl_Name, Names, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.
                  JSON_Content_Handler'Class;
      Decl_Part : LAL.Declarative_Part;
      Item      : Text_Edit_Map;
      Success   : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Decl_Part.As_Ada_Node, Success);

      Write_Removable_Decl_Part (Writer, Success);

      Writer.End_Object (Success);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.
                  JSON_Content_Handler'Class;
      Decl_Name : LAL.Defining_Name;
      Item      : Text_Edit_Map;
      Success   : in out Boolean) is
   begin
      Writer.Start_Object (Success);

      Writer.Key_Name ("children", Success);
      Writer.Start_Array (Success);
      Writer.End_Array (Success);

      Write_Fixit (Writer, Item, Success);

      Write_Warning (Writer, Success);
      Writer.Key_Name ("locations", Success);

      Write_Node (Writer, Decl_Name.As_Ada_Node, Success);

      Write_Message_Private (Writer, Decl_Name, Success);

      Writer.End_Object (Success);
   end Write;

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Delete_Infos;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class)
   is
      Writer     : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success    : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Record_Node in Edits_Info.Texts_Edit.Iterate loop
         Write (Writer       => Writer,
                Record_Name  => Get_Record_Name (Record_Node.Key),
                Delete_Names => Edits_Info.Deletable_Names
                                (Get_Record_Name (Record_Node.Key)),
                Item         => Edits_Info.Texts_Edit (Record_Node),
                Success      => Success);
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Aggregates_To_Edit_Text.Map;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class)
   is
      Writer     : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success    : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Aggregate_Node in Edits_Info.Iterate loop
         Write (Writer         => Writer,
                Aggregate_Node => Aggregate_Node.Key,
                Item           => Edits_Info (Aggregate_Node),
                Success        => Success);
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Tools.Suppress_Dead_Params_Tool.Edit_Infos;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class)
   is
      Writer     : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success    : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Func_Name in Edits_Info.Removable_Params.Iterate loop
         if not Edits_Info.Removable_Params (Func_Name).Is_Empty then
            Write (Writer        => Writer,
                   Function_Name => Func_Name.Key.F_Subp_Name,
                   Item          => Edits_Info.Text_Info (Func_Name.Key),
                   Params        => Edits_Info.Removable_Params (Func_Name),
                   Success       => Success);
         end if;
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Tools.Scope_Declarations_Tool.Modify_Info;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class) is
      Writer     : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success    : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Obj in Edits_Info.Edit_Info.Iterate loop
         Write (Writer        => Writer,
                Obj_Decl_Name => Obj.Key,
                Item          => Edits_Info.Edit_Info (Obj),
                Names         => Edits_Info.Object_To_Decl (Obj.Key),
                Success       => Success);
      end loop;
      for Decl_Part in Edits_Info.Removable_Decl_Part.Iterate loop
         Write (Writer    => Writer,
                Decl_Part => Decl_Part.Key,
                Item      => Edits_Info.Removable_Decl_Part (Decl_Part),
                Success   => Success);
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Tools.Relocate_Decls_Tool.Modify_Info;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class) is
      Writer     : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success    : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Obj in Edits_Info.Edit_Of_Obj_Decl.Iterate loop
         Write (Writer        => Writer,
                Obj_Decl_Name => Obj.Key,
                Item          => Edits_Info.Edit_Of_Obj_Decl (Obj),
                Names         => Edits_Info.Object_To_Names (Obj.Key),
                Success       => Success);
      end loop;
      for Decl in Edits_Info.Edit_Of_Other_Decl.Iterate loop
         Write (Writer    => Writer,
                Decl_Name => Decl.Key,
                Item      => Edits_Info.Edit_Of_Other_Decl (Decl),
                Success   => Success);
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;
end Output;

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

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer    : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Item      : Source_Location;
      File_Name : String;
      Success   : in out Boolean) is
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
      Success   : in out Boolean) is
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
      Success : in out Boolean) is
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
      Success  : in out Boolean) is
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
      Success   : in out Boolean) is
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

   ------------------
   -- Write_Record --
   ------------------

   procedure Write_Node
     (Writer         : in out VSS.JSON.Content_Handlers
                       .JSON_Content_Handler'Class;
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
      Success : in out Boolean) is
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
      Success      : in out Boolean) is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The component(s) ";
      for Name of Delete_Names loop
         Words := Words & "'" & Text.Image (Name.Text) & "' ";
      end loop;
      Words := Words & "in the record '"
           & Text.Image (Record_Name.Text) &
        "' can be remove";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Writer         : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Aggregate_Node : LAL.Aggregate'Class;
      Success        : in out Boolean) is
      Words : Unbounded_String := Null_Unbounded_String;
   begin
      Writer.Key_Name ("message", Success);
      Words := Words & "The Aggregate ";
      Words := Words & Text.Image (Aggregate_Node.Text) &
        " Array aggregate using () is an obsolescent syntax, use [] instead.";
      Writer.String_Value (To_Virtual_String (Words), Success);
   end Write_Message;

   -----------
   -- Write --
   -----------

   procedure Write
     (Writer       : in out VSS.JSON.Content_Handlers.
                           JSON_Content_Handler'Class;
      Record_Name  : LAL.Defining_Name'Class;
      Delete_Names : Defining_Name_Ordered_Sets.Set;
      Item         : Text_Edit_Map;
      Success      : in out Boolean) is
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
      Success        : in out Boolean) is
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

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Delete_Infos;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class) is
      Writer  : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Record_Node in Edits_Info.Texts_Edit.Iterate loop
         Write (Writer,
                Get_Record_Name (Record_Node.Key),
                Edits_Info.Deletable_Names (Get_Record_Name (Record_Node.Key)),
                Edits_Info.Texts_Edit (Record_Node),
                Success);
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;

   --------------------
   -- JSON_Serialize --
   --------------------

   procedure JSON_Serialize
     (Edits_Info : Aggregates_To_Edit_Text.Map;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class) is
      Writer  : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success : Boolean := True;
   begin
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document (Success);
      Writer.Start_Array (Success);
      for Aggregate_Node in Edits_Info.Iterate loop
         Write (Writer,
                Aggregate_Node.Key,
                Edits_Info (Aggregate_Node),
                Success);
      end loop;
      Writer.End_Array (Success);
      Writer.End_Document (Success);
   end JSON_Serialize;

end Output;

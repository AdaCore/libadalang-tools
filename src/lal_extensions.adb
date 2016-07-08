with Ada.Wide_Wide_Characters.Handling;

with Libadalang.Analysis; use Libadalang.Analysis;

package body LAL_Extensions is

   use Ada_Node_Vecs;

   function Childx
     (Node  : access Ada_Node_Type'Class;
      Index : Positive) return Ada_Node
   is
   begin
      return Child (Ada_Node (Node), Index);
   end Childx;

   procedure Find_Iter
     (Node      : access Ada_Node_Type'Class;
      Predicate : not null access function
        (Node : access Ada_Node_Type'Class) return Boolean;
      Visit     : not null access procedure
        (Node : access Ada_Node_Type'Class))
   is

      function Visit_If_Predicate
        (Node : access Ada_Node_Type'Class) return Visit_Status;

      function Visit_If_Predicate
        (Node : access Ada_Node_Type'Class) return Visit_Status is
      begin
         if Predicate (Node) then
            Visit (Node);
         end if;

         return Into;
      end Visit_If_Predicate;
   begin
      Traverse (Node, Visit_If_Predicate'Access);
   end Find_Iter;

   procedure Find_Iter
     (Node      : access Ada_Node_Type'Class;
      Node_Kind : Ada_Node_Kind_Type;
      Visit     : not null access procedure
        (Node : access Ada_Node_Type'Class))
   is

      function Kind_Equal
        (Node : access Ada_Node_Type'Class) return Boolean is
        (Kind (Node) = Node_Kind);
   begin
      Find_Iter (Node, Kind_Equal'Access, Visit);
   end Find_Iter;

   function Find_All
     (Node      : access Ada_Node_Type'Class;
      Predicate : not null access function
        (Node : access Ada_Node_Type'Class) return Boolean)
      return Ada_Node_Vecs.Elements_Array
   is

      Result_Vector : Ada_Node_Vecs.Vector;

      procedure Append (Node : access Ada_Node_Type'Class);

      procedure Append (Node : access Ada_Node_Type'Class) is
      begin
         Append (Result_Vector, Node);
      end Append;
   begin
      Find_Iter (Node, Predicate, Append'Access);

      return Result : constant Ada_Node_Vecs.Elements_Array :=
        To_Array (Result_Vector)
      do
         Destroy (Result_Vector);
      end return;
   end Find_All;

   function Find_All
     (Node      : access Ada_Node_Type'Class;
      Node_Kind : Ada_Node_Kind_Type) return Ada_Node_Vecs.Elements_Array
   is

      Result_Vector : Ada_Node_Vecs.Vector;

      procedure Append (Node : access Ada_Node_Type'Class);

      procedure Append (Node : access Ada_Node_Type'Class) is
      begin
         Append (Result_Vector, Node);
      end Append;
   begin
      Find_Iter (Node, Node_Kind, Append'Access);

      return Result : constant Ada_Node_Vecs.Elements_Array :=
        To_Array (Result_Vector)
      do
         Destroy (Result_Vector);
      end return;
   end Find_All;

   function Id_Name
     (Nm : access Ada_Node_Type'Class)
     return Text_Type
   is
   begin
      return Data (F_Tok (Single_Tok_Node (Nm))).Text.all;
   end Id_Name;

   function L_Name
     (Nm : access Ada_Node_Type'Class)
     return Text_Type
   is
      use Ada.Wide_Wide_Characters.Handling;
   begin
      return To_Lower (Id_Name (Nm));
   end L_Name;

   function Full_Name (Nm : Name) return Text_Type is
   begin
      case Kind (Nm) is
         when Ada_Dotted_Name =>
            --  ????Not sure why we have to convert to Name here:
            return Full_Name (Name (F_Prefix (Dotted_Name (Nm)))) &
              "." & Full_Name (Name (F_Suffix (Dotted_Name (Nm))));
         when Ada_Identifier | Ada_String_Literal =>
            return Data (F_Tok (Single_Tok_Node (Nm))).Text.all;

         when others =>
            raise Program_Error with
              "Full_Name of " & Short_Image (Ada_Node (Nm));
      end case;
   end Full_Name;

   function Get_Def_Name (Decl : Ada_Node) return Name is
   begin
      return Result : Name do
         case Kind (Decl) is
            when Ada_Compilation_Unit =>
               Result :=
                 Get_Def_Name (F_Body (Compilation_Unit (Decl)));
            when Ada_Library_Item =>
               Result :=
                 Get_Def_Name (Ada_Node (F_Item (Library_Item (Decl))));
            when Ada_Subunit =>
               Result :=
                 Get_Def_Name (Ada_Node (F_Body (Subunit (Decl))));

            when Ada_Generic_Function_Instantiation |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Procedure_Instantiation =>
               Result :=
                 F_Name (Generic_Instantiation (Decl));
            when Ada_Generic_Renaming_Decl =>
               Result :=
                 F_Name (Generic_Renaming_Decl (Decl));
            when Ada_Package_Body_Stub =>
               Result :=
                 F_Name (Package_Body_Stub (Decl));
            when Ada_Package_Renaming_Decl =>
               Result :=
                 F_Name (Package_Renaming_Decl (Decl));
            when Ada_Accept_Statement =>
               Result :=
                 Name (F_Name (Accept_Statement (Decl)));
            when Ada_Block_Statement =>
               Result :=
                 Name (F_Name (Block_Statement (Decl)));
            when Ada_Loop_Statement =>
               Result :=
                 Name (F_Name (Loop_Statement (Decl)));
            when Ada_Abstract_Subprogram_Decl |
              Ada_Expression_Function |
              Ada_Null_Subprogram_Decl |
              Ada_Renaming_Subprogram_Decl |
              Ada_Subprogram_Decl =>
               Result :=
                 F_Name (F_Subp_Spec (Basic_Subprogram_Decl (Decl)));
            when Ada_Package_Decl =>
               Result :=
                 F_Package_Name (Base_Package_Decl (Decl));
            when Ada_Generic_Package_Decl =>
               Result :=
                 F_Package_Name
                 (F_Package_Decl (Generic_Package_Decl (Decl)));
            when Ada_Generic_Subprogram_Decl =>
               Result :=
                 F_Name (F_Subp_Spec (Generic_Subprogram_Decl (Decl)));
            when Ada_Package_Body =>
               Result :=
                 F_Package_Name (Package_Body (Decl));
            when Ada_Subprogram_Body =>
               Result :=
                 F_Name (F_Subp_Spec (Subprogram_Body (Decl)));
            when Ada_Protected_Decl =>
               Result :=
                 Name (F_Protected_Name (Protected_Decl (Decl)));
            when Ada_Protected_Type_Decl =>
               Result :=
                 Name (F_Protected_Type_Name (Protected_Type_Decl (Decl)));
            when Ada_Protected_Body =>
               Result := F_Name (Protected_Body (Decl));
            when Ada_Entry_Body =>
               Result :=
                 Name (F_Entry_Name (Entry_Body (Decl)));
            when Ada_Task_Decl =>
               Result :=
                 Name (F_Task_Name (Task_Decl (Decl)));
            when Ada_Task_Type_Decl =>
               Result :=
                 Name (F_Task_Type_Name (Task_Type_Decl (Decl)));
            when Ada_Task_Body =>
               Result := F_Name (Task_Body (Decl));
            when others =>
               raise Program_Error with
                 "Get_Def_Name of " & Short_Image (Decl);
         end case;

         if Decl.all in Basic_Decl_Type then
            --  Should Subprogram_Decl_Type be in Basic_Decl_Type????
            declare
               D : constant Name_Array_Access :=
                 P_Defining_Names (Basic_Decl (Decl));
               --  ????Free
            begin
               pragma Assert (D.N = 1);
               pragma Assert (Result = D.Items (1));
            end;
         end if;
      end return;
   end Get_Def_Name;

   function Get_Aspects (Decl : Basic_Decl) return Aspect_Specification is
   begin
      return (case Kind (Decl) is
         when Ada_Base_Package_Decl =>
            F_Aspects (Base_Package_Decl (Decl)),
         when Ada_Abstract_Subprogram_Decl =>
            F_Aspects (Abstract_Subprogram_Decl (Decl)),
         when Ada_Expression_Function =>
            F_Aspects (Expression_Function (Decl)),
         when Ada_Null_Subprogram_Decl =>
            F_Aspects (Null_Subprogram_Decl (Decl)),
         when Ada_Renaming_Subprogram_Decl =>
            F_Aspects (Renaming_Subprogram_Decl (Decl)),
         when Ada_Subprogram_Decl =>
            F_Aspects (Subprogram_Decl (Decl)),
         when Ada_Package_Body_Stub =>
            F_Aspects (Package_Body_Stub (Decl)),
         when Ada_Subprogram_Body_Stub =>
            F_Aspects (Subprogram_Body_Stub (Decl)),
         when Ada_Task_Body_Stub =>
            F_Aspects (Task_Body_Stub (Decl)),
         when Ada_Package_Body =>
            F_Aspects (Package_Body (Decl)),
         when Ada_Protected_Body =>
            F_Aspects (Protected_Body (Decl)),
         when Ada_Subprogram_Body =>
            F_Aspects (Subprogram_Body (Decl)),
         when Ada_Task_Body =>
            F_Aspects (Task_Body (Decl)),
         when Ada_Exception_Decl =>
            F_Aspects (Exception_Decl (Decl)),
         when Ada_Generic_Function_Instantiation |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Procedure_Instantiation =>
            F_Aspects (Generic_Instantiation (Decl)),
         when Ada_Generic_Renaming_Decl =>
            F_Aspects (Generic_Renaming_Decl (Decl)),
         when Ada_Generic_Subprogram_Decl =>
            F_Aspects (Generic_Subprogram_Decl (Decl)),
         when Ada_Object_Decl =>
            F_Aspects (Object_Decl (Decl)),
         when Ada_Package_Renaming_Decl =>
            F_Aspects (Package_Renaming_Decl (Decl)),
         when Ada_Protected_Decl =>
            F_Aspects (Protected_Decl (Decl)),
         when Ada_Protected_Type_Decl =>
            F_Aspects (Protected_Type_Decl (Decl)),
         when Ada_Task_Decl =>
            F_Aspects (Task_Decl (Decl)),
         when Ada_Task_Type_Decl =>
            F_Aspects (Task_Type_Decl (Decl)),
         when Ada_Full_Type_Decl =>
            F_Aspects (Full_Type_Decl (Decl)),
         when Ada_Subtype_Decl =>
            F_Aspects (Subtype_Decl (Decl)),
--  See P415-048:
--         when ada_Component_Decl =>
--            F_Aspects (Component_Decl (Decl)),
--         when ada_Entry_Decl =>
--            F_Aspects (Entry_Decl (Decl)),
--         when ada_Protected_Body_Stub =>
--            F_Aspects (Protected_Body_Stub (Decl)),
         when others => raise Program_Error);
   end Get_Aspects;

   function Visible_Part
     (Node : access Ada_Node_Type'Class) return List_Ada_Node
   is
      --  I'm confused about Base_Package_Decl
      --  Maybe return the Public_Part node?
   begin
      case Kind (Node) is
         when Ada_Package_Decl =>
            return F_Decls (F_Public_Part (Base_Package_Decl (Node)));
         when Ada_Generic_Package_Decl =>
            return F_Decls
              (F_Public_Part (F_Package_Decl (Generic_Package_Decl (Node))));
         when Ada_Task_Def =>
            return F_Decls (F_Public_Part (Task_Def (Node)));
         when Ada_Protected_Def =>
            return F_Decls (F_Public_Part (Protected_Def (Node)));
         when others => raise Program_Error;
      end case;
   end Visible_Part;

end LAL_Extensions;

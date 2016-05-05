with Ada.Wide_Wide_Characters.Handling;

with Libadalang.Analysis; use Libadalang.Analysis;

package body LAL_Extensions is

   use Ada_Node_Vecs;

   function Childx
     (Node  : access Ada_Node_Type'Class;
      Index : Natural) return Ada_Node
   is
   begin
      return Child (Ada_Node (Node), Index);
   end Childx;

   procedure Find_Iter
     (Node      : Ada_Node;
      Predicate : not null access function (Node : Ada_Node) return Boolean;
      Visit     : not null access procedure (Node : Ada_Node))
   is

      function Visit_If_Predicate (Node : Ada_Node) return Visit_Status;

      function Visit_If_Predicate (Node : Ada_Node) return Visit_Status is
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
     (Node      : Ada_Node;
      Node_Kind : Ada_Node_Type_Kind;
      Visit     : not null access procedure (Node : Ada_Node))
   is

      function Kind_Equal
        (Node : Ada_Node) return Boolean is
        (Kind (Node) = Node_Kind);
   begin
      Find_Iter (Node, Kind_Equal'Access, Visit);
   end Find_Iter;

   function Find_All
     (Node      : Ada_Node;
      Predicate : not null access function (Node : Ada_Node) return Boolean)
      return Ada_Node_Vecs.Elements_Array
   is

      Result_Vector : Ada_Node_Vecs.Vector;

      procedure Append (Node : Ada_Node);

      procedure Append (Node : Ada_Node) is
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
     (Node      : Ada_Node;
      Node_Kind : Ada_Node_Type_Kind) return Ada_Node_Vecs.Elements_Array
   is

      Result_Vector : Ada_Node_Vecs.Vector;

      procedure Append (Node : Ada_Node);

      procedure Append (Node : Ada_Node) is
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
         when Prefix_Kind =>
            --  ????Not sure why we have to convert to Name here:
            return Full_Name (Name (F_Prefix (Prefix (Nm)))) &
              "." & Full_Name (Name (F_Suffix (Prefix (Nm))));
         when Identifier_Kind | String_Literal_Kind =>
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
            when Compilation_Unit_Kind =>
               Result :=
                 Get_Def_Name (F_Body (Compilation_Unit (Decl)));
            when Library_Item_Kind =>
               Result :=
                 Get_Def_Name (Ada_Node (F_Item (Library_Item (Decl))));
            when Subunit_Kind =>
               Result :=
                 Get_Def_Name (Ada_Node (F_Body (Subunit (Decl))));

            when Generic_Function_Instantiation_Kind |
              Generic_Package_Instantiation_Kind |
              Generic_Procedure_Instantiation_Kind =>
               Result :=
                 F_Name (Generic_Instantiation (Decl));
            when Generic_Renaming_Decl_Kind =>
               Result :=
                 F_Name (Generic_Renaming_Decl (Decl));
            when Package_Body_Stub_Kind =>
               Result :=
                 F_Name (Package_Body_Stub (Decl));
            when Package_Renaming_Decl_Kind =>
               Result :=
                 F_Name (Package_Renaming_Decl (Decl));
            when Accept_Statement_Kind =>
               Result :=
                 Name (F_Name (Accept_Statement (Decl)));
            when Block_Statement_Kind =>
               Result :=
                 Name (F_Name (Block_Statement (Decl)));
            when Loop_Statement_Kind =>
               Result :=
                 Name (F_Name (Loop_Statement (Decl)));
            when Abstract_Subprogram_Decl_Kind |
              Expression_Function_Kind |
              Null_Subprogram_Decl_Kind |
              Renaming_Subprogram_Decl_Kind |
              Subprogram_Decl_Kind =>
               Result :=
                 F_Name (F_Subp_Spec (Basic_Subprogram_Decl (Decl)));
            when Package_Decl_Kind =>
               Result :=
                 F_Package_Name (Base_Package_Decl (Decl));
            when Generic_Package_Decl_Kind =>
               Result :=
                 F_Package_Name
                 (F_Package_Decl (Generic_Package_Decl (Decl)));
            when Generic_Subprogram_Decl_Kind =>
               Result :=
                 F_Name (F_Subp_Spec (Generic_Subprogram_Decl (Decl)));
            when Package_Body_Kind =>
               Result :=
                 F_Package_Name (Package_Body (Decl));
            when Subprogram_Body_Kind =>
               Result :=
                 F_Name (F_Subp_Spec (Subprogram_Body (Decl)));
            when Protected_Decl_Kind =>
               Result :=
                 Name (F_Protected_Name (Protected_Decl (Decl)));
            when Protected_Type_Decl_Kind =>
               Result :=
                 Name (F_Protected_Type_Name (Protected_Type_Decl (Decl)));
            when Protected_Body_Kind =>
               Result :=
                 F_Package_Name (Protected_Body (Decl)); -- package????
            when Entry_Body_Kind =>
               Result :=
                 Name (F_Entry_Name (Entry_Body (Decl)));
            when Task_Decl_Kind =>
               Result :=
                 Name (F_Task_Name (Task_Decl (Decl)));
            when Task_Type_Decl_Kind =>
               Result :=
                 Name (F_Task_Type_Name (Task_Type_Decl (Decl)));
            when Task_Body_Kind =>
               Result :=
                 F_Package_Name (Task_Body (Decl)); -- package????
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
         when Base_Package_Decl_Kind =>
            F_Aspects (Base_Package_Decl (Decl)),
         when Abstract_Subprogram_Decl_Kind =>
            F_Aspects (Abstract_Subprogram_Decl (Decl)),
         when Expression_Function_Kind =>
            F_Aspects (Expression_Function (Decl)),
         when Null_Subprogram_Decl_Kind =>
            F_Aspects (Null_Subprogram_Decl (Decl)),
         when Renaming_Subprogram_Decl_Kind =>
            F_Aspects (Renaming_Subprogram_Decl (Decl)),
         when Subprogram_Decl_Kind =>
            F_Aspects (Subprogram_Decl (Decl)),
         when Package_Body_Stub_Kind =>
            F_Aspects (Package_Body_Stub (Decl)),
         when Subprogram_Body_Stub_Kind =>
            F_Aspects (Subprogram_Body_Stub (Decl)),
         when Task_Body_Stub_Kind =>
            F_Aspects (Task_Body_Stub (Decl)),
         when Package_Body_Kind =>
            F_Aspects (Package_Body (Decl)),
         when Protected_Body_Kind =>
            F_Aspects (Protected_Body (Decl)),
         when Subprogram_Body_Kind =>
            F_Aspects (Subprogram_Body (Decl)),
         when Task_Body_Kind =>
            F_Aspects (Task_Body (Decl)),
         when Exception_Decl_Kind =>
            F_Aspects (Exception_Decl (Decl)),
         when Generic_Function_Instantiation_Kind |
              Generic_Package_Instantiation_Kind |
              Generic_Procedure_Instantiation_Kind =>
            F_Aspects (Generic_Instantiation (Decl)),
         when Generic_Renaming_Decl_Kind =>
            F_Aspects (Generic_Renaming_Decl (Decl)),
         when Generic_Subprogram_Decl_Kind =>
            F_Aspects (Generic_Subprogram_Decl (Decl)),
         when Object_Decl_Kind =>
            F_Aspects (Object_Decl (Decl)),
         when Package_Renaming_Decl_Kind =>
            F_Aspects (Package_Renaming_Decl (Decl)),
         when Protected_Decl_Kind =>
            F_Aspects (Protected_Decl (Decl)),
         when Protected_Type_Decl_Kind =>
            F_Aspects (Protected_Type_Decl (Decl)),
         when Task_Decl_Kind =>
            F_Aspects (Task_Decl (Decl)),
         when Task_Type_Decl_Kind =>
            F_Aspects (Task_Type_Decl (Decl)),
         when Full_Type_Decl_Kind =>
            F_Aspects (Full_Type_Decl (Decl)),
         when Subtype_Decl_Kind =>
            F_Aspects (Subtype_Decl (Decl)),
--  See P415-048:
--         when Component_Decl_Kind =>
--            F_Aspects (Component_Decl (Decl)),
--         when Entry_Decl_Kind =>
--            F_Aspects (Entry_Decl (Decl)),
--         when Protected_Body_Stub_Kind =>
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
         when Package_Decl_Kind =>
            return F_Decls (F_Public_Part (Base_Package_Decl (Node)));
         when Generic_Package_Decl_Kind =>
            return F_Decls
              (F_Public_Part (F_Package_Decl (Generic_Package_Decl (Node))));
         when Task_Def_Kind =>
            return F_Decls (F_Public_Part (Task_Def (Node)));
         when Protected_Def_Kind =>
            return F_Decls (F_Public_Part (Protected_Def (Node)));
         when others => raise Program_Error;
      end case;
   end Visible_Part;

end LAL_Extensions;

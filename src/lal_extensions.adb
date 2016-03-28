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

   function Full_Name (Name : Expr) return Text_Type is
   begin
      case Kind (Name) is
         when Prefix_Kind =>
            return Full_Name (F_Prefix (Prefix (Name))) &
              "." & Full_Name (F_Suffix (Prefix (Name)));
         when Identifier_Kind =>
            return F_Tok (Single_Tok_Node (Name)).Text.all;
         when others =>
            raise Program_Error;
      end case;
   end Full_Name;

   function Name (Decl : Ada_Node) return Expr is
   begin
      case Kind (Decl) is
         when Compilation_Unit_Kind =>
            pragma Assert
              (Child_Count (F_Bodies (Compilation_Unit (Decl))) = 1);
            return Name (Childx (F_Bodies (Compilation_Unit (Decl)), 0));
         when Library_Item_Kind =>
            return Name (F_Item (Library_Item (Decl)));

         when Generic_Instantiation_Kind =>
            return F_Name (Generic_Instantiation (Decl));
         when Generic_Renaming_Decl_Kind =>
            return F_Name (Generic_Renaming_Decl (Decl));
         when Package_Body_Stub_Kind =>
            return F_Name (Package_Body_Stub (Decl));
         when Package_Renaming_Decl_Kind =>
            return F_Name (Package_Renaming_Decl (Decl));
         when Accept_Statement_Kind =>
            return Expr (F_Name (Accept_Statement (Decl)));
         when Block_Statement_Kind =>
            return Expr (F_Name (Block_Statement (Decl)));
         when Loop_Statement_Kind =>
            return Expr (F_Name (Loop_Statement (Decl)));
         when Subprogram_Decl_Kind =>
            return F_Name (F_Subp_Spec (Subprogram_Decl (Decl)));
         when Subunit_Kind =>
            return F_Name (Subunit (Decl));
         when Package_Decl_Kind | Generic_Package_Decl_Kind =>
            return F_Package_Name (Base_Package_Decl (Decl));
         when Package_Body_Kind =>
            return F_Package_Name (Package_Body (Decl));
         when Subprogram_Body_Kind =>
            return F_Name (F_Subp_Spec (Subprogram_Body (Decl)));
         when others =>
            raise Program_Error with "Name of " & Short_Image (Decl);
      end case;
   end Name;

end LAL_Extensions;

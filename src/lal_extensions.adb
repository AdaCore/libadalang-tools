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

   function Full_Name (Nm : Name) return Text_Type is
   begin
      case Kind (Nm) is
         when Prefix_Kind =>
            --  ????Not sure why we have to convert to Name here:
            return Full_Name (Name (F_Prefix (Prefix (Nm)))) &
              "." & Full_Name (Name (F_Suffix (Prefix (Nm))));
         when Identifier_Kind | String_Literal_Kind =>
            return F_Tok (Single_Tok_Node (Nm)).Text.all;

         when others =>
            raise Program_Error with
              "Full_Name of " & Short_Image (Ada_Node (Nm));
      end case;
   end Full_Name;

   function Get_Name (Decl : Ada_Node) return Name is
   begin
      return Result : Name do
         case Kind (Decl) is
            when Compilation_Unit_Kind =>
--            pragma Assert
--              (Child_Count (F_Bodies (Compilation_Unit (Decl))) = 1);
               Result :=
                 Get_Name (Childx (F_Bodies (Compilation_Unit (Decl)), 0));
            when Library_Item_Kind =>
               Result :=
                 Get_Name (F_Item (Library_Item (Decl)));

            when Generic_Instantiation_Kind =>
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
            when Subprogram_Decl_Kind =>
               Result :=
                 F_Name (F_Subp_Spec (Subprogram_Decl (Decl)));
            when Subunit_Kind =>
               Result :=
                 F_Name (Subunit (Decl));
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
               raise Program_Error with "Get_Name of " & Short_Image (Decl);
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
   end Get_Name;

end LAL_Extensions;

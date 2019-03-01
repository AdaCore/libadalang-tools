with Ada.Characters.Conversions; use Ada;
with Ada.Wide_Characters.Handling;

package body LAL_Extensions is

   use Ada_Node_Vectors;

   function Childx (Node : Ada_Node'Class; Index : Positive) return Ada_Node is
   begin
      return Child (Ada_Node (Node), Index);
   end Childx;

   function Contains_Kind
     (Node : Ada_Node'Class; Kind : Ada_Node_Kind_Type) return Boolean
   is
      function Visit (Node : Ada_Node'Class) return Visit_Status is
        (if Node.Kind = Kind then Stop else Into);
   begin
      return Traverse (Node, Visit'Access) = Stop;
   end Contains_Kind;

   procedure Find_Iter
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean;
      Visit : not null access procedure (Node : Ada_Node'Class))
   is

      function Visit_If_Predicate (Node : Ada_Node'Class) return Visit_Status;

      function Visit_If_Predicate (Node : Ada_Node'Class) return Visit_Status
      is
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
     (Node  : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type;
      Visit : not null access procedure (Node : Ada_Node'Class))
   is

      function Kind_Equal (Node : Ada_Node'Class) return Boolean is
        (Kind (Node) = Node_Kind);
   begin
      Find_Iter (Node, Kind_Equal'Access, Visit);
   end Find_Iter;

   function Find_All
     (Node      : Ada_Node'Class;
      Predicate : not null access function
        (Node : Ada_Node'Class) return Boolean)
      return Ada_Node_Array
   is

      Result_Vector : Ada_Node_Vector;

      procedure Append (Node : Ada_Node'Class);

      procedure Append (Node : Ada_Node'Class) is
      begin
         Append (Result_Vector, Node.As_Ada_Node);
      end Append;
   begin
      Find_Iter (Node, Predicate, Append'Access);

      return To_Array (Result_Vector);
   end Find_All;

   function Find_All
     (Node : Ada_Node'Class; Node_Kind : Ada_Node_Kind_Type)
      return Ada_Node_Array
   is

      Result_Vector : Ada_Node_Vector;

      procedure Append (Node : Ada_Node'Class);

      procedure Append (Node : Ada_Node'Class) is
      begin
         Append (Result_Vector, Node.As_Ada_Node);
      end Append;
   begin
      Find_Iter (Node, Node_Kind, Append'Access);

      return To_Array (Result_Vector);
   end Find_All;

   function Token_Text (Tok : Token_Reference) return W_Str is
   begin
      return Text_To_W_Str (Text (Tok));
   end Token_Text;

   function L_Token_Text (Tok : Token_Reference) return W_Str is
      use Ada.Wide_Characters.Handling;
   begin
      return To_Lower (Token_Text (Tok));
   end L_Token_Text;

   function Id_Name (Nm : Ada_Node'Class) return W_Str is
   begin
      if Nm.Kind = Ada_Defining_Name then
         return Id_Name (Nm.As_Defining_Name.F_Name);
      else
         return Text_To_W_Str (Text (Nm.As_Single_Tok_Node));
      end if;
   end Id_Name;

   function L_Name (Nm : Ada_Node'Class) return W_Str is
      use Ada.Wide_Characters.Handling;
   begin
      return To_Lower (Id_Name (Nm));
   end L_Name;

   function Label_Name (L : Ada_Node'Class) return W_Str is
   begin
      return Text_To_W_Str (Text (F_Name (F_Decl (L.As_Label))));
   end Label_Name;

   function Full_Name (Nm : Name) return W_Str is
   begin
      case Kind (Nm) is
         when Ada_Dotted_Name =>
            return Full_Name (Nm.As_Dotted_Name.F_Prefix) & "." &
              Full_Name (Nm.As_Dotted_Name.F_Suffix.As_Name);
         when Ada_Identifier | Ada_String_Literal =>
            return Id_Name (Nm);

         when Ada_Defining_Name =>
            return Full_Name (Nm.As_Defining_Name.F_Name);

         when others =>
            raise Program_Error
              with "Full_Name of " & Short_Image (Nm.As_Ada_Node);
      end case;
   end Full_Name;

   function L_Full_Name (Nm : Name) return W_Str is
      use Ada.Wide_Characters.Handling;
   begin
      return To_Lower (Full_Name (Nm));
   end L_Full_Name;

   function Is_Def_Name (Node : Base_Id'Class) return Boolean is
   begin
      return Node.Parent.Kind in Ada_Basic_Decl
        and then
        (for some Id of Node.Parent.As_Basic_Decl.P_Defining_Names =>
           Node = Id);
   end Is_Def_Name;

   function Get_Def_Name (Decl : Ada_Node'Class) return Defining_Name is
   begin
      case Kind (Decl) is
         when Ada_Compilation_Unit =>
            return Get_Def_Name (Decl.As_Compilation_Unit.F_Body);
         when Ada_Library_Item =>
            return Get_Def_Name (Decl.As_Library_Item.F_Item);
         when Ada_Subunit =>
            return Get_Def_Name (Decl.As_Subunit.F_Body);
         when Ada_Basic_Decl =>
            declare
               D : constant Defining_Name_Array :=
                 Decl.As_Basic_Decl.P_Defining_Names;
            begin
               pragma Assert (D'Length = 1);
               return D (1);
            end;
         when others =>
            raise Program_Error with "Get_Def_Name of " & Short_Image (Decl);
      end case;
   end Get_Def_Name;

   function Get_Aspects (Decl : Basic_Decl) return Aspect_Spec is
   begin
      return
        (case Kind (Decl) is
           when Ada_Base_Package_Decl => Decl.As_Base_Package_Decl.F_Aspects,
           when Ada_Abstract_Subp_Decl => Decl.As_Abstract_Subp_Decl.F_Aspects,
           when Ada_Expr_Function => Decl.As_Expr_Function.F_Aspects,
           when Ada_Null_Subp_Decl => Decl.As_Null_Subp_Decl.F_Aspects,
           when Ada_Subp_Renaming_Decl => Decl.As_Subp_Renaming_Decl.F_Aspects,
           when Ada_Subp_Decl                  => Decl.As_Subp_Decl.F_Aspects,
           when Ada_Package_Body_Stub => Decl.As_Package_Body_Stub.F_Aspects,
           when Ada_Subp_Body_Stub => Decl.As_Subp_Body_Stub.F_Aspects,
           when Ada_Task_Body_Stub => Decl.As_Task_Body_Stub.F_Aspects,
           when Ada_Package_Body => Decl.As_Package_Body.F_Aspects,
           when Ada_Protected_Body => Decl.As_Protected_Body.F_Aspects,
           when Ada_Subp_Body                  => Decl.As_Subp_Body.F_Aspects,
           when Ada_Task_Body                  => Decl.As_Task_Body.F_Aspects,
           when Ada_Exception_Decl => Decl.As_Exception_Decl.F_Aspects,
           when Ada_Generic_Subp_Instantiation =>
             Decl.As_Generic_Subp_Instantiation.F_Aspects,
           when Ada_Generic_Package_Instantiation =>
             Decl.As_Generic_Package_Instantiation.F_Aspects,
           when Ada_Generic_Package_Renaming_Decl =>
             Decl.As_Generic_Package_Renaming_Decl.F_Aspects,
           when Ada_Generic_Subp_Renaming_Decl =>
             Decl.As_Generic_Subp_Renaming_Decl.F_Aspects,
           when Ada_Generic_Subp_Decl =>
             Decl.As_Generic_Subp_Decl.F_Subp_Decl.F_Aspects,
           when Ada_Object_Decl           => Decl.As_Object_Decl.F_Aspects,
           when Ada_Package_Renaming_Decl =>
             Decl.As_Package_Renaming_Decl.F_Aspects,
           when Ada_Single_Protected_Decl =>
             Decl.As_Single_Protected_Decl.F_Aspects,
           when Ada_Protected_Type_Decl =>
             Decl.As_Protected_Type_Decl.F_Aspects,
           when Ada_Single_Task_Decl =>
             Decl.As_Single_Task_Decl.F_Task_Type.F_Aspects,
           --  The aspects are on the anonymous task type.

           when Ada_Task_Type_Decl => Decl.As_Task_Type_Decl.F_Aspects,
           when Ada_Type_Decl      => Decl.As_Type_Decl.F_Aspects,
           when Ada_Subtype_Decl   => Decl.As_Subtype_Decl.F_Aspects,
           --  See P415-048:
--         when ada_Component_Decl =>
--            F_Aspects (Component_Decl (Decl)),
--         when ada_Entry_Decl =>
--            F_Aspects (Entry_Decl (Decl)),
--         when ada_Protected_Body_Stub =>
--            F_Aspects (Protected_Body_Stub (Decl)),

           when others => raise Program_Error);
   end Get_Aspects;

   function G_Formal_Part (Node : Ada_Node'Class) return Generic_Formal_Part is
   begin
      case Kind (Node) is
         when Ada_Generic_Package_Decl =>
            return Node.As_Generic_Package_Decl.F_Formal_Part;
         when Ada_Generic_Subp_Decl =>
            return Node.As_Generic_Subp_Decl.F_Formal_Part;
         when others =>
            raise Program_Error;
      end case;
   end G_Formal_Part;

   function Vis_Part (Node : Ada_Node'Class) return Public_Part is
   --  I'm confused about Base_Package_Decl
      begin
      case Kind (Node) is
         when Ada_Package_Decl =>
            return Node.As_Base_Package_Decl.F_Public_Part;
         when Ada_Generic_Package_Decl =>
            return Node.As_Generic_Package_Decl.F_Package_Decl.F_Public_Part;
         when Ada_Task_Def =>
            return Node.As_Task_Def.F_Public_Part;
         when Ada_Protected_Def =>
            return Node.As_Protected_Def.F_Public_Part;
         when Ada_Single_Task_Decl => -- This will recurse 2 levels!
            return Vis_Part (Node.As_Single_Task_Decl.F_Task_Type);
         when Ada_Task_Type_Decl =>
            return Vis_Part (Node.As_Task_Type_Decl.F_Definition);
         when Ada_Single_Protected_Decl => -- This will recurse 2 levels!
            return Vis_Part (Node.As_Single_Protected_Decl.F_Definition);
         --  Why doesn't this parallel tasks???
            when Ada_Protected_Type_Decl =>
            return Vis_Part (Node.As_Protected_Type_Decl.F_Definition);
         when others =>
            raise Program_Error;
      end case;
   end Vis_Part;

   function Priv_Part (Node : Ada_Node'Class) return Private_Part is
   begin
      case Kind (Node) is
         when Ada_Package_Decl =>
            return Node.As_Base_Package_Decl.F_Private_Part;
         when Ada_Generic_Package_Decl =>
            return Node.As_Generic_Package_Decl.F_Package_Decl.F_Private_Part;
         when Ada_Task_Def =>
            return Node.As_Task_Def.F_Private_Part;
         when Ada_Protected_Def =>
            return Node.As_Protected_Def.F_Private_Part;
         when Ada_Single_Task_Decl => -- This will recurse 2 levels!
            return Priv_Part (Node.As_Single_Task_Decl.F_Task_Type);
         when Ada_Task_Type_Decl =>
            return Priv_Part (Node.As_Task_Type_Decl.F_Definition);
         when Ada_Single_Protected_Decl => -- This will recurse 2 levels!
            return Priv_Part (Node.As_Single_Protected_Decl.F_Definition);
         --  Why doesn't this parallel tasks???
            when Ada_Protected_Type_Decl =>
            return Priv_Part (Node.As_Protected_Type_Decl.F_Definition);
         when others =>
            raise Program_Error;
      end case;
   end Priv_Part;

   function Body_Decls (Node : Ada_Node'Class) return Declarative_Part is
   begin
      case Kind (Node) is
         when Ada_Entry_Body =>
            return Node.As_Entry_Body.F_Decls;
         when Ada_Package_Body =>
            return Node.As_Package_Body.F_Decls;
         when Ada_Protected_Body =>
            return Node.As_Protected_Body.F_Decls;
         when Ada_Subp_Body =>
            return Node.As_Subp_Body.F_Decls;
         when Ada_Task_Body =>
            return Node.As_Task_Body.F_Decls;
         when others =>
            raise Program_Error;
      end case;
   end Body_Decls;

   function Is_Program_Unit (Node : Ada_Node) return Boolean is
   begin
      case Kind (Node) is
         when Ada_Entry_Decl =>
            return Kind (Parent (Parent (Parent (Node)))) = Ada_Protected_Def;

         when Ada_Task_Type_Decl | Ada_Protected_Type_Decl |
           Ada_Single_Task_Decl | Ada_Single_Protected_Decl | Ada_Subp_Decl |
           Ada_Abstract_Subp_Decl |
           --  But not Ada_Null_Subp_Decl
            Ada_Subp_Body | Ada_Package_Decl |
           Ada_Package_Body | Ada_Task_Body | Ada_Protected_Body |
           Ada_Entry_Body | Ada_Subp_Body_Stub | Ada_Package_Body_Stub |
           Ada_Task_Body_Stub | Ada_Protected_Body_Stub |
           Ada_Generic_Subp_Decl | Ada_Generic_Package_Decl =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Program_Unit;

   function Get_Subp_Spec (Node : Ada_Node'Class) return Subp_Spec is
   begin
      case Kind (Node) is
         when Ada_Classic_Subp_Decl =>
            return Node.As_Classic_Subp_Decl.F_Subp_Spec;
         when Ada_Generic_Subp_Decl =>
            return Node.As_Generic_Subp_Decl.F_Subp_Decl.F_Subp_Spec;
         when Ada_Subp_Body_Stub =>
            return Node.As_Subp_Body_Stub.F_Subp_Spec;
         when Ada_Base_Subp_Body =>
            return Node.As_Base_Subp_Body.F_Subp_Spec;
         when Ada_Access_To_Subp_Def =>
            return Node.As_Access_To_Subp_Def.F_Subp_Spec;
         when others =>
            raise Program_Error;
      end case;
   end Get_Subp_Spec;

   function Xref (Node : Ada_Node'Class) return Defining_Name is
   begin
      case Node.Kind is
         when Ada_Identifier =>
            return Node.As_Identifier.P_Xref;
         when Ada_Dotted_Name =>
            return Xref (Node.As_Dotted_Name.F_Suffix);
         when others =>
            raise Program_Error;
      end case;
   end Xref;

   function Adds_New_Nesting_Level (Node : Ada_Node) return Boolean is
   begin
      case Kind (Node) is
         when Ada_Subp_Body | Ada_Package_Decl | Ada_Package_Body |
           Ada_Task_Body | Ada_Protected_Body | Ada_Entry_Body |
           Ada_Generic_Package_Decl | Ada_Begin_Block | Ada_Decl_Block |
           Ada_Case_Stmt | Ada_Select_Stmt | Ada_For_Loop_Stmt |
           Ada_Loop_Stmt | Ada_While_Loop_Stmt | Ada_Accept_Stmt |
           Ada_Accept_Stmt_With_Stmts | Ada_If_Stmt =>
            return True;

         when others =>
            return False;
      end case;
   end Adds_New_Nesting_Level;

   function Text_To_W_Str (X : Text_Type) return W_Str is
   begin
      pragma Assert (Characters.Conversions.Is_Wide_String (X));
      return Characters.Conversions.To_Wide_String (X);
   end Text_To_W_Str;

end LAL_Extensions;

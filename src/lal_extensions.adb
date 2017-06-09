with Ada.Characters.Conversions; use Ada;
with Ada.Wide_Characters.Handling;

package body LAL_Extensions is

   use Ada_Node_Vectors;

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
      return Ada_Node_Array
   is

      Result_Vector : Ada_Node_Vector;

      procedure Append (Node : access Ada_Node_Type'Class);

      procedure Append (Node : access Ada_Node_Type'Class) is
      begin
         Append (Result_Vector, Node);
      end Append;
   begin
      Find_Iter (Node, Predicate, Append'Access);

      return To_Array (Result_Vector);
   end Find_All;

   function Find_All
     (Node      : access Ada_Node_Type'Class;
      Node_Kind : Ada_Node_Kind_Type) return Ada_Node_Array
   is

      Result_Vector : Ada_Node_Vector;

      procedure Append (Node : access Ada_Node_Type'Class);

      procedure Append (Node : access Ada_Node_Type'Class) is
      begin
         Append (Result_Vector, Node);
      end Append;
   begin
      Find_Iter (Node, Node_Kind, Append'Access);

      return To_Array (Result_Vector);
   end Find_All;

   function Token_Text (Tok : Token_Type) return W_Str is
   begin
      return Text_To_W_Str (Text (Tok));
   end Token_Text;

   function L_Token_Text (Tok : Token_Type) return W_Str is
      use Ada.Wide_Characters.Handling;
   begin
      return To_Lower (Token_Text (Tok));
   end L_Token_Text;

   function Id_Name
     (Nm : access Ada_Node_Type'Class)
     return W_Str
   is
   begin
      return Text_To_W_Str (Text (F_Tok (Single_Tok_Node (Nm))));
   end Id_Name;

   function L_Name
     (Nm : access Ada_Node_Type'Class)
     return W_Str
   is
      use Ada.Wide_Characters.Handling;
   begin
      return To_Lower (Id_Name (Nm));
   end L_Name;

   function Label_Name
     (L : access Ada_Node_Type'Class)
     return W_Str
   is
   begin
      return Text_To_W_Str (Text (F_Name (F_Decl (Label (L)))));
   end Label_Name;

   function Full_Name (Nm : Name) return W_Str is
   begin
      case Kind (Nm) is
         when Ada_Dotted_Name =>
            return Full_Name (F_Prefix (Dotted_Name (Nm))) &
              "." & Full_Name (Name (F_Suffix (Dotted_Name (Nm))));
         when Ada_Identifier | Ada_String_Literal =>
            return Id_Name (Nm);

         when others =>
            raise Program_Error with
              "Full_Name of " & Short_Image (Ada_Node (Nm));
      end case;
   end Full_Name;

   function L_Full_Name (Nm : Name) return W_Str is
      use Ada.Wide_Characters.Handling;
   begin
      return To_Lower (Full_Name (Nm));
   end L_Full_Name;

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

            when Ada_Generic_Subp_Instantiation =>
               Result :=
                 F_Subp_Name (Generic_Subp_Instantiation (Decl));
            when Ada_Generic_Package_Instantiation =>
               Result :=
                 F_Name (Generic_Package_Instantiation (Decl));
            when Ada_Generic_Package_Renaming_Decl =>
               Result :=
                 F_Name (Generic_Package_Renaming_Decl (Decl));
            when Ada_Generic_Subp_Renaming_Decl =>
               Result :=
                 F_Name (Generic_Subp_Renaming_Decl (Decl));
            when Ada_Package_Body_Stub =>
               Result :=
                 F_Name (Package_Body_Stub (Decl));
            when Ada_Subp_Body_Stub =>
               Result :=
                 F_Subp_Name (Get_Subp_Spec (Decl));
            when Ada_Task_Body_Stub =>
               Result :=
                 F_Name (Task_Body_Stub (Decl));
            when Ada_Protected_Body_Stub =>
               Result :=
                 F_Name (Protected_Body_Stub (Decl));
            when Ada_Package_Renaming_Decl =>
               Result :=
                 F_Name (Package_Renaming_Decl (Decl));
            when Ada_Accept_Stmt =>
               Result :=
                 Name (F_Name (Accept_Stmt (Decl)));
            when Ada_Accept_Stmt_With_Stmts =>
               Result :=
                 Name (F_Name (Accept_Stmt_With_Stmts (Decl)));
            when Ada_Named_Stmt_Decl =>
               Result := Name (F_Name (Named_Stmt_Decl (Decl)));
            when Ada_Abstract_Subp_Decl |
              Ada_Expr_Function |
              Ada_Null_Subp_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Subp_Decl =>
               Result :=
                 F_Subp_Name (Get_Subp_Spec (Decl));
            when Ada_Package_Decl =>
               Result :=
                 F_Package_Name (Base_Package_Decl (Decl));
            when Ada_Generic_Package_Decl =>
               Result :=
                 F_Package_Name
                 (F_Package_Decl (Generic_Package_Decl (Decl)));
            when Ada_Generic_Subp_Decl =>
               Result :=
                 F_Subp_Name (Get_Subp_Spec (Decl));
            when Ada_Package_Body =>
               Result :=
                 F_Package_Name (Package_Body (Decl));
            when Ada_Subp_Body =>
               Result :=
                 F_Subp_Name (Get_Subp_Spec (Decl));
            when Ada_Single_Protected_Decl =>
               Result :=
                 Name (F_Protected_Name (Single_Protected_Decl (Decl)));
            when Ada_Protected_Type_Decl =>
               Result :=
                 Name (F_Protected_Type_Name (Protected_Type_Decl (Decl)));
            when Ada_Protected_Body =>
               Result := F_Name (Protected_Body (Decl));
            when Ada_Entry_Decl =>
               Result :=
                 Name (F_Entry_Id (Entry_Decl (Decl)));
            when Ada_Entry_Body =>
               Result :=
                 Name (F_Entry_Name (Entry_Body (Decl)));
            when Ada_Single_Task_Decl =>
               Result :=
                 Name (F_Type_Id (F_Task_Type (Single_Task_Decl (Decl))));
            when Ada_Task_Type_Decl =>
               Result :=
                 Name (F_Type_Id (Task_Type_Decl (Decl)));
            when Ada_Task_Body =>
               Result := F_Name (Task_Body (Decl));
            when others =>
               raise Program_Error with
                 "Get_Def_Name of " & Short_Image (Decl);
         end case;

         if Decl.all in Basic_Decl_Type then
            --  Should Subp_Decl_Type be in Basic_Decl_Type????
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

   function Get_Aspects (Decl : Basic_Decl) return Aspect_Spec is
   begin
      return (case Kind (Decl) is
         when Ada_Base_Package_Decl =>
            F_Aspects (Base_Package_Decl (Decl)),
         when Ada_Abstract_Subp_Decl =>
            F_Aspects (Abstract_Subp_Decl (Decl)),
         when Ada_Expr_Function =>
            F_Aspects (Expr_Function (Decl)),
         when Ada_Null_Subp_Decl =>
            F_Aspects (Null_Subp_Decl (Decl)),
         when Ada_Subp_Renaming_Decl =>
            F_Aspects (Subp_Renaming_Decl (Decl)),
         when Ada_Subp_Decl =>
            F_Aspects (Subp_Decl (Decl)),
         when Ada_Package_Body_Stub =>
            F_Aspects (Package_Body_Stub (Decl)),
         when Ada_Subp_Body_Stub =>
            F_Aspects (Subp_Body_Stub (Decl)),
         when Ada_Task_Body_Stub =>
            F_Aspects (Task_Body_Stub (Decl)),
         when Ada_Package_Body =>
            F_Aspects (Package_Body (Decl)),
         when Ada_Protected_Body =>
            F_Aspects (Protected_Body (Decl)),
         when Ada_Subp_Body =>
            F_Aspects (Subp_Body (Decl)),
         when Ada_Task_Body =>
            F_Aspects (Task_Body (Decl)),
         when Ada_Exception_Decl =>
            F_Aspects (Exception_Decl (Decl)),
         when Ada_Generic_Subp_Instantiation =>
            F_Aspects (Generic_Subp_Instantiation (Decl)),
         when Ada_Generic_Package_Instantiation =>
            F_Aspects (Generic_Package_Instantiation (Decl)),
         when Ada_Generic_Package_Renaming_Decl =>
            F_Aspects (Generic_Package_Renaming_Decl (Decl)),
         when Ada_Generic_Subp_Renaming_Decl =>
            F_Aspects (Generic_Subp_Renaming_Decl (Decl)),
         when Ada_Generic_Subp_Decl =>
            F_Aspects (Generic_Subp_Decl (Decl)),
         when Ada_Object_Decl =>
            F_Aspects (Object_Decl (Decl)),
         when Ada_Package_Renaming_Decl =>
            F_Aspects (Package_Renaming_Decl (Decl)),
         when Ada_Single_Protected_Decl =>
            F_Aspects (Single_Protected_Decl (Decl)),
         when Ada_Protected_Type_Decl =>
            F_Aspects (Protected_Type_Decl (Decl)),
         when Ada_Single_Task_Decl =>
            F_Aspects (F_Task_Type (Single_Task_Decl (Decl))),
            --  The aspects are on the anonymous task type.
         when Ada_Task_Type_Decl =>
            F_Aspects (Task_Type_Decl (Decl)),
         when Ada_Type_Decl =>
            F_Aspects (Type_Decl (Decl)),
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

   function G_Formal_Part
     (Node : access Ada_Node_Type'Class) return Generic_Formal_Part is
   begin
      case Kind (Node) is
         when Ada_Generic_Package_Decl =>
            return F_Formal_Part (Generic_Package_Decl (Node));
         when Ada_Generic_Subp_Decl =>
            return F_Formal_Part (Generic_Subp_Decl (Node));
         when others => raise Program_Error;
      end case;
   end G_Formal_Part;

   function Vis_Part
     (Node : access Ada_Node_Type'Class) return Public_Part
   is
      --  I'm confused about Base_Package_Decl
   begin
      case Kind (Node) is
         when Ada_Package_Decl =>
            return F_Public_Part (Base_Package_Decl (Node));
         when Ada_Generic_Package_Decl =>
            return
              F_Public_Part (F_Package_Decl (Generic_Package_Decl (Node)));
         when Ada_Task_Def =>
            return F_Public_Part (Task_Def (Node));
         when Ada_Protected_Def =>
            return F_Public_Part (Protected_Def (Node));
         when Ada_Single_Task_Decl => -- This will recurse 2 levels!
            return Vis_Part (F_Task_Type (Single_Task_Decl (Node)));
         when Ada_Task_Type_Decl =>
            return Vis_Part (F_Definition (Task_Type_Decl (Node)));
         when Ada_Single_Protected_Decl => -- This will recurse 2 levels!
            return Vis_Part (F_Definition (Single_Protected_Decl (Node)));
            --  Why doesn't this parallel tasks???
         when Ada_Protected_Type_Decl =>
            return Vis_Part (F_Definition (Protected_Type_Decl (Node)));
         when others => raise Program_Error;
      end case;
   end Vis_Part;

   function Priv_Part
     (Node : access Ada_Node_Type'Class) return Private_Part is
   begin
      case Kind (Node) is
         when Ada_Package_Decl =>
            return F_Private_Part (Base_Package_Decl (Node));
         when Ada_Generic_Package_Decl =>
            return
              F_Private_Part (F_Package_Decl (Generic_Package_Decl (Node)));
         when Ada_Task_Def =>
            return F_Private_Part (Task_Def (Node));
         when Ada_Protected_Def =>
            return F_Private_Part (Protected_Def (Node));
         when Ada_Single_Task_Decl => -- This will recurse 2 levels!
            return Priv_Part (F_Task_Type (Single_Task_Decl (Node)));
         when Ada_Task_Type_Decl =>
            return Priv_Part (F_Definition (Task_Type_Decl (Node)));
         when Ada_Single_Protected_Decl => -- This will recurse 2 levels!
            return Priv_Part (F_Definition (Single_Protected_Decl (Node)));
            --  Why doesn't this parallel tasks???
         when Ada_Protected_Type_Decl =>
            return Priv_Part (F_Definition (Protected_Type_Decl (Node)));
         when others => raise Program_Error;
      end case;
   end Priv_Part;

   function Body_Decls
     (Node : access Ada_Node_Type'Class) return Declarative_Part is
   begin
      case Kind (Node) is
         when Ada_Entry_Body => return F_Decls (Entry_Body (Node));
         when Ada_Package_Body => return F_Decls (Package_Body (Node));
         when Ada_Protected_Body => return F_Decls (Protected_Body (Node));
         when Ada_Subp_Body => return F_Decls (Subp_Body (Node));
         when Ada_Task_Body => return F_Decls (Task_Body (Node));
         when others => raise Program_Error;
      end case;
   end Body_Decls;

   function Is_Program_Unit (Node : Ada_Node) return Boolean is
   begin
      case Kind (Node) is
         when Ada_Entry_Decl =>
            return Kind (Parent (Parent (Parent (Node)))) = Ada_Protected_Def;

         when Ada_Task_Type_Decl |
           Ada_Protected_Type_Decl |
           Ada_Single_Task_Decl |
           Ada_Single_Protected_Decl |
           Ada_Subp_Decl |
           Ada_Abstract_Subp_Decl |
           --  But not Ada_Null_Subp_Decl
           Ada_Subp_Body |
           Ada_Package_Decl |
           Ada_Package_Body |
           Ada_Task_Body |
           Ada_Protected_Body |
           Ada_Entry_Body |
           Ada_Subp_Body_Stub |
           Ada_Package_Body_Stub |
           Ada_Task_Body_Stub |
           Ada_Protected_Body_Stub |
           Ada_Generic_Subp_Decl |
           Ada_Generic_Package_Decl =>
            return True;

         when others => return False;
      end case;
   end Is_Program_Unit;

   function Get_Subp_Spec (Node : Ada_Node) return Subp_Spec is
   begin
      case Kind (Node) is
         when Ada_Classic_Subp_Decl =>
            return F_Subp_Spec (Classic_Subp_Decl (Node));
         when Ada_Generic_Subp_Decl =>
            return F_Subp_Spec (Generic_Subp_Decl (Node));
         when Ada_Subp_Body_Stub =>
            return F_Subp_Spec (Subp_Body_Stub (Node));
         when Ada_Subp_Body =>
            return F_Subp_Spec (Subp_Body (Node));
         when Ada_Access_To_Subp_Def =>
            return F_Subp_Spec (Access_To_Subp_Def (Node));
         when others => raise Program_Error;
      end case;
   end Get_Subp_Spec;

   function Adds_New_Nesting_Level (Node : Ada_Node) return Boolean is
   begin
      case Kind (Node) is
         when Ada_Subp_Body |
           Ada_Package_Decl |
           Ada_Package_Body |
           Ada_Task_Body |
           Ada_Protected_Body |
           Ada_Entry_Body |
           Ada_Generic_Package_Decl |

           Ada_Begin_Block |
           Ada_Decl_Block |
           Ada_Case_Stmt |
           Ada_Select_Stmt |
           Ada_For_Loop_Stmt |
           Ada_Loop_Stmt |
           Ada_While_Loop_Stmt |
           Ada_Accept_Stmt |
           Ada_Accept_Stmt_With_Stmts |
           Ada_If_Stmt =>
            return True;

         when others => return False;
      end case;
   end Adds_New_Nesting_Level;

   function Text_To_W_Str (X : Text_Type) return W_Str is
   begin
      pragma Assert (Characters.Conversions.Is_Wide_String (X));
      return Characters.Conversions.To_Wide_String (X);
   end Text_To_W_Str;

end LAL_Extensions;

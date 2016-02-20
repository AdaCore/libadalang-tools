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

end LAL_Extensions;

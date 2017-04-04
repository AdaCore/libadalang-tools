package body C413002q is
   procedure Prim_Proc (X : in out Tq) is
   begin
      X.Data := 20;
   end Prim_Proc;

   procedure Prim_Proc (X : in out Tq; Value : Integer) is
   begin
      X.Data := 2 * Value;
   end Prim_Proc;

   function Prim_Func (X : Tq) return Integer is
   begin
      return 3;
   end Prim_Func;

   function Prim_Func (X : Tq; Value : Integer) return Integer is
   begin
      return 2 * Value;
   end Prim_Func;

   procedure Class_Wide_Proc (X : in out Tq'Class; Value : Float) is
   begin
      X.Value := 3.0 * Value;
   end Class_Wide_Proc;

   function Class_Wide_Func (X : Tq'Class; Value : Float) return Float is
   begin
      return 3.0 * Value;
   end Class_Wide_Func;

   function Prim_New_Func (X : Tq) return Integer is
   begin
      return -4;
   end Prim_New_Func;

   package body Local is
      procedure Prim_Proc (X : in out Tpp) is
      begin
         X.Data := 100;
      end Prim_Proc;

      procedure Prim_Proc (X : in out Tpp; Value : Integer) is
      begin
         X.Data := 4 * Value;
      end Prim_Proc;

      function Prim_Func (X : Tpp) return Integer is
      begin
         return 102;
      end Prim_Func;

      function Prim_Func (X : Tpp; Value : Integer) return Integer is
      begin
         return 4 * Value;
      end Prim_Func;
   end Local;
end C413002q;

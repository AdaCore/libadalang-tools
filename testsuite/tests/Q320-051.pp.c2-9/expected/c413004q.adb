package body C413004q is
   procedure Prim_Proc (X : access Tq) is
   begin
      X.Data := 20;
   end Prim_Proc;

   procedure Prim_Proc (X : access Tq; Value : Integer) is
   begin
      X.Data := 2 * Value;
   end Prim_Proc;

   function Prim_Func (X : access constant Tq) return Integer is
   begin
      return 3;
   end Prim_Func;

   function Prim_Func (X : access constant Tq; Value : Integer) return Integer
   is
   begin
      return 2 * Value;
   end Prim_Func;

   procedure Class_Wide_Proc (X : access Tq'Class; Value : Float) is
   begin
      X.Value := 3.0 * Value;
   end Class_Wide_Proc;

   function Class_Wide_Func
     (X : access constant Tq'Class; Value : Float) return Float
   is
   begin
      return 3.0 * Value;
   end Class_Wide_Func;

   function Prim_New_Func (X : access constant Tq) return Integer is
   begin
      return -4;
   end Prim_New_Func;

   package body Local is
      procedure Prim_Proc (X : access Tpp) is
      begin
         X.Data := 100;
      end Prim_Proc;

      procedure Prim_Proc (X : access Tpp; Value : Integer) is
      begin
         X.Data := 4 * Value;
      end Prim_Proc;

      function Prim_Func (X : access constant Tpp) return Integer is
      begin
         return 102;
      end Prim_Func;

      function Prim_Func
        (X : access constant Tpp; Value : Integer) return Integer
      is
      begin
         return 4 * Value;
      end Prim_Func;
   end Local;
end C413004q;

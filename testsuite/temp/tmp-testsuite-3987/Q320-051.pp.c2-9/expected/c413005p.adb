package body C413005p is

   procedure Clear (X : in out Tp) is
   begin
      X.Data := 0;
   end Clear;

   procedure Set (X : in out Tp; Value : Integer) is
   begin
      X.Data := Value;
   end Set;

   function Get (X : Tp) return Integer is
   begin
      return X.Data;
   end Get;

   function Prod (X : Tp; Value : Integer) return Integer is
   begin
      return X.Data * Value;
   end Prod;

   procedure Class_Wide_Clear (X : in out Tp'Class) is
   begin
      X.Data := 0;
   end Class_Wide_Clear;

   procedure Class_Wide_Set (X : in out Tp'Class; Value : Integer) is
   begin
      X.Data := Value;
   end Class_Wide_Set;

   function Class_Wide_Get (X : Tp'Class) return Integer is
   begin
      return X.Data;
   end Class_Wide_Get;

   function Class_Wide_Prod (X : Tp'Class; Value : Integer) return Integer is
   begin
      return X.Data * Value;
   end Class_Wide_Prod;
end C413005p;

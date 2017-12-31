with C413003p;
package C413003q is
   type Tq is new C413003p.Tp with record
      Value : Float := 0.0;
   end record;

   procedure Prim_Proc (X : access Tq);
   procedure Prim_Proc (X : access Tq; Value : Integer);
   function Prim_Func (X : access constant Tq) return Integer;
   function Prim_Func (X : access constant Tq; Value : Integer) return Integer;

   procedure Class_Wide_Proc (X : access Tq'Class; Value : Float);
   function Class_Wide_Func (X : access constant Tq'Class;
      Value                    : Float) return Float;
   --  Note: Formals of these class-wide subprograms are different from the
   --        class-wide subprograms defined in the ancestor.

   function Prim_New_Func (X : access constant Tq) return Integer;
   --  This is a new primitive operation.

   package Local is
      type Tpp is new Tq with null record;
      procedure Prim_Proc (X : access Tpp);
      procedure Prim_Proc (X : access Tpp; Value : Integer);
      function Prim_Func (X : access constant Tpp) return Integer;
      function Prim_Func (X : access constant Tpp;
         Value              : Integer) return Integer;
   end Local;
end C413003q;

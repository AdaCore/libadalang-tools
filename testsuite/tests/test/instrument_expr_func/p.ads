package P is

   function Expr (X : Integer) return Integer is
     (X * 10);

   function Expr_Private_Body (X : Integer) return Integer;

   procedure Package_Needs_Body (X : in out Integer);

   package N1 is
      function Expr_Nested (X : Integer) return Integer is
        (X * 40);
      procedure Nested_Needs_Body;
   end N1;

   package N2 is
      package N22 is
         function Expr_Nested_no_package_Body (X : Integer) return Integer is
            (X - 3);
      end N22;

      package N23 is
         function Expr_Nested_no_package_Body_2 (X : Integer) return Integer is
            (X + 3);
      end N23;

   end N2;

private
   function Expr_Private_Body (X : Integer) return Integer is (X - 3);
   Foo : Boolean := True;
end P;

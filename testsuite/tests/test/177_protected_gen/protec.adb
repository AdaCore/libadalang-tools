package body Protec is

   protected body P_Typ is
      entry Foo (X : Integer) when Update is
      begin
         Val := X;
      end Foo;
      procedure Bar (X : Integer) is
      begin
         Val := X;
      end Bar;
      function Baz (X : Integer) return Integer is
      begin
         return Val - X;
      end Baz;

      procedure P_Bar (X : Integer) is
      begin
         Val := X;
      end P_Bar;
      function P_Baz (X : Integer) return Integer is
      begin
         return Val - X;
      end P_Baz;
   end P_Typ;

   protected body Single_P is
      entry Foo (X : Integer) when Update is
      begin
         Val := X;
      end Foo;
      procedure Bar (X : Integer) is
      begin
         Val := X;
      end Bar;
      function Baz (X : Integer) return Integer is
      begin
         return Val - X;
      end Baz;

      procedure P_Bar (X : Integer) is
      begin
         Val := X;
      end P_Bar;
      function P_Baz (X : Integer) return Integer is
      begin
         return Val - X;
      end P_Baz;
   end Single_P;

   function Foo (X : Integer) return Integer is
   begin
      return X;
   end Foo;

end Protec;

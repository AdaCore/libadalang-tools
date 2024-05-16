package Protec is

   protected Type P_Typ is
      entry Foo (X : Integer);
      procedure Bar (X : Integer);
      function Baz (X : Integer) return Integer;
   private
      Val    : Integer;
      Update : Boolean := False;
      procedure P_Bar (X : Integer);
      function P_Baz (X : Integer) return Integer;
   end P_Typ;

   protected Single_P is
      entry Foo (X : Integer);
      procedure Bar (X : Integer);
      function Baz (X : Integer) return Integer;
   private
      Val    : Integer;
      Update : Boolean;
      procedure P_Bar (X : Integer);
      function P_Baz (X : Integer) return Integer;
   end Single_P;

   function Foo (X : Integer) return Integer;

end Protec;

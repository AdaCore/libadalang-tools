with Priv;
package Pkg is
   type T1 is new Priv.T;
   type T2 is new T1;

   type U1 is new Priv.U;
   type U2 is new U1;

   subtype S1 is Priv.T;
   type S2 is new S1;
   subtype S3 is S2;
   type S4 is new S3;

   function Foo (X : T2) return T2;
   function Bar (X : U2) return U2;
   function Baz (X : S1) return S1;
   function Qux (X : S2) return S2;
   function Rol (X : S3) return S3;
   function Pal (X : S4) return S4;

end Pkg;

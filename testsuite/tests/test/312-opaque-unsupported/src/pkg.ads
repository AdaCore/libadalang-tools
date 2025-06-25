with Priv;

package Pkg is

   type Opaque is new Priv.Priv_T;

   function Should_Be_Rejected (Val : Opaque) return Opaque;

   function Should_Be_Accepted (Val : Integer) return Integer;

end Pkg;

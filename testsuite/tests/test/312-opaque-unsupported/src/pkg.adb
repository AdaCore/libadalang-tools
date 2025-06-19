package body Pkg is

   function Should_Be_Rejected (Val : Opaque) return Opaque is
   begin
      return Val;
   end Should_Be_Rejected;

   function Should_Be_Accepted (Val : Integer) return Integer is
   begin
      return Val;
   end Should_Be_Accepted;

end Pkg;

package Priv.Child is
   type R is private;
   function Foo (Val : R) return Integer;

private
   type R is record
      Val : Priv.T;
   end record;
   function Foo (Val : R) return Integer
   is (Integer (Val.Val));

end Priv.Child;

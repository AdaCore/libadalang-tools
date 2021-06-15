package Test is
   function Foo return Boolean is (True);
   function Bar return Boolean is (Foo);
   procedure Baz;
end Test;

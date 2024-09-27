with Foo; use Foo;

package Test is

   function "&" (L : Integer; R : Foo.Test) return Foo.Test is (R);

end Test;

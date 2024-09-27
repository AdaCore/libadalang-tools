with Foo; use Foo;

package Test is

   type My_Int is new Integer;

   procedure Test (X : My_Int; Y : Integer; Z : Foo.Test);

end Test;

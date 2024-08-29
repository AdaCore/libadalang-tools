generic
   type T is range <>;
   Val : T;
package Gen is
   procedure Params (X : T);
   function Ret return T;
end Gen;
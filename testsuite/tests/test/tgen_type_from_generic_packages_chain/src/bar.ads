generic
   type Element_Type is private;
package Bar is

   function Identity (X : Element_Type) return Element_Type;

end Bar;

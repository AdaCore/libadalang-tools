generic
   type Int is new Big_Int;
package Ca15003a.Pure is
   pragma Pure;
   function F (X : access Int) return Int;
end Ca15003a.Pure;

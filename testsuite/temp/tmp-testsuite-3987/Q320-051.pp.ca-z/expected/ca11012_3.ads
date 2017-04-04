-- No body for CA11012_2;

     --==================================================================--

-- Declare instances of the generic complex packages for integer type.
-- The instance of the child must itself be declared as a child of the
-- instance of the parent.

with Ca11012_0;                        -- Complex number abstraction
with Ca11012_2;                        -- Package containing integer type
pragma Elaborate (Ca11012_0);
package Ca11012_3 is new Ca11012_0 (Int_Type => Ca11012_2.My_Integer);

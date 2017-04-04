--==================================================================--

-- Declare instances of the generic complex packages for real type. The
-- instance of the child must itself be declared as a child of the instance
-- of the parent.

with Ca11013_0;                       -- Complex number.
with Ca11013_2;                       -- Random number generator.
pragma Elaborate (Ca11013_0);
package Ca11013_3 is new Ca11013_0
  (Random_Generator => Ca11013_2.Random_Complex,
   Real_Type => Ca11013_2.My_Float);

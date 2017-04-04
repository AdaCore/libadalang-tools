--==================================================================--

-- Private generic child of Complex_Number.

private
 generic

-- No parameter.

package Ca11021_0.Ca11021_1 is

   -- ... Other declarations.

   -- Low level operation on complex number.
   function "+" (Left, Right : Complex_Type) return Complex_Type;

   function "-" (Right : Complex_Type) return Complex_Type;

   -- ... Various other operations in real application.

end Ca11021_0.Ca11021_1;

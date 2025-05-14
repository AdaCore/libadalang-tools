package Priv is

   type T is private;
   type U is private;

private

   type T is new Integer;
   type U is new Float;

end Priv;

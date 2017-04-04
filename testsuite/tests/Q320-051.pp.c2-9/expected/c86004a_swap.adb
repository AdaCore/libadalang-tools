procedure C86004a_Swap (X, Y : in out Item) is
   T : Item;
begin
   T := X;
   X := Y;
   Y := T;
end C86004a_Swap;

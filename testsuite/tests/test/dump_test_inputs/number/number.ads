package Number is

   type Number_Type is new Integer range -100 .. 100;

   Overflow : Boolean := False;

end Number;

package Pb_Arrays is

   --  Check that generation of an array type with an index type that is
   --  actually a signed integer subtype with a constraint works. TGen used
   --  to skip the constraint, which resulted in wrong values being produced.
   
   type Array_Type is array (Integer range <>) of Character;
   subtype Item_Nb is Integer range 1..20;
   subtype Item_Array is Array_Type (Item_Nb);

   --  Same for an enumeration type
      
   type Weekdays is (Monday, Tuesday, Wednesday, Thursday);
   type Calendar is array (Weekdays range <>) of Character;
   subtype TW is Weekdays range Tuesday .. Wednesday;
   subtype Beg_Calendar is Calendar (TW);
   
   type Box_Type is
      record
         Nb    : Item_Nb;
         Data  : Item_Array;
         Data2 : Beg_Calendar;
      end record;
   
   procedure Proc (E : Box_Type);
   
end Pb_Arrays;

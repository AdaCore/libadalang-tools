package Foo is

   type Larger_Record_Type is record
      A : Integer;
      B : Float;
      C : Boolean;
      D : Character;
      E : String (1 .. 27);
   end record;

   function Test_23_Larger_Record_Type (Test_Data_1 : Larger_Record_Type;
                                         Throw_Exception : Boolean := True)
                                         return Boolean is (True);
end Foo;

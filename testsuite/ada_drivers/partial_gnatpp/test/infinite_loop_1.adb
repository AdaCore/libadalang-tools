procedure Infinite_Loop_1 is

   --  type A is
   --  ;

   aggr : My_Type := My_Type'(
                             );

   function My_Func (A, B : Boolean) return Boolean is
     (A and not B);

   A, Val : Boolean := False;
   B      : Boolean := False;
begin

   B := My_Func (A, );


   null;

end Infinite_Loop_1;

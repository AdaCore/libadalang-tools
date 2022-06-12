package Pack is
   type Some_Type is tagged private;
   procedure Do_Smt (X : access Some_Type);
private
   type Some_Type is tagged record
      Component : Integer;
   end record;
end Pack;

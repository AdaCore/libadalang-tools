procedure Split_Lines_Before_Vertical_Bars
is

   type Enum_1_Type is
      (Enum_1_Red,
       Enum_1_Green,
       Enum_1_Yellow,
       Enum_1_Orange,
       Enum_1_Cyan,
       Enum_1_White);
   V : Enum_1_Type;

begin
   case V is
      when Enum_1_Red
         | Enum_1_Green
         | Enum_1_Yellow
         | Enum_1_Orange
         | Enum_1_Cyan
         | Enum_1_White =>
         null;
   end case;
end Split_Lines_Before_Vertical_Bars;

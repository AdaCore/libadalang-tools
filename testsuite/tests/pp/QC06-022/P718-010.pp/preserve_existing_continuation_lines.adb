procedure Preserve_Existing_Continuation_Lines
is

   type Array_Type is
      array (Integer,
             Integer,
             Integer,
             Integer)
      of Boolean;
   Value                   : Array_Type;
   Index_Long_Identifier_1 : Integer;
   Index_Long_Identifier_2 : Integer;
   Index_Long_Identifier_3 : Integer;
   Index_Long_Identifier_4 : Integer;
   Result                  : Boolean;
   Boolean_Value_1         : Boolean;
   Boolean_Value_2         : Boolean;

begin
   Result := Value (Index_Long_Identifier_1,
                    Index_Long_Identifier_2,
                    Index_Long_Identifier_3,
                    Index_Long_Identifier_4);

   if Boolean_Value_1
      and then Boolean_Value_2
   then
      null;
   end if;
end Preserve_Existing_Continuation_Lines;

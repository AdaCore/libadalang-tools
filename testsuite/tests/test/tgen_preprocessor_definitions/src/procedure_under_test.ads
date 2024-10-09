package Procedure_Under_Test is

   Some_Public_String : String := $debug_string;

   #if debug
   Some_Public_Value_1 : Integer := 0;
   #else
   Some_Public_Value_1 : Integer := 200;
   #end if;

   #if debug_symbol="true"
   Some_Public_Value_2 : Integer := 0;
   #else
   Some_Public_Value_2 : Integer := 200;
   #end if;

   procedure Test (Some_Value : Integer);

end Procedure_Under_Test;

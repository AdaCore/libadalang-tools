with Text_IO; use Text_IO;

package body Procedure_Under_Test is

   procedure Test (Some_Value : Integer) is
   begin
      #if debug
      #if debug_symbol="true"
      if Some_Value > 0 and Some_Public_Value_1  = 0 and Some_Public_Value_2 = 0 then
         Put_Line (Some_Public_String);
      end if;
      #else
      Put_Line (Some_Public_String);
      #end if;
      #else
      Put_Line (Test_Failed);
      #end if;
   end Test;

end Procedure_Under_Test;

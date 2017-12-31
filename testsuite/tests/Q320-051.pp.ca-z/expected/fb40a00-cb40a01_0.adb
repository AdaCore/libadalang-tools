     --=================================================================--

with Report;

package body Fb40a00.Cb40a01_0 is

   procedure Process_Text (Text : in String_Pointer_Type) is
      Pos : Natural := Text'First - 1;
   begin
      loop   -- Process string, raise exception upon completion.
         Pos := Pos + 1;
         if Pos > Text.all'Last then
            raise Completed_Text_Processing;
         elsif (Text.all (Pos) in 'A' .. 'Z') or
           (Text.all (Pos) in 'a' .. 'z') or (Text.all (Pos) in '0' .. '9')
         then
            Increment_Alphanumeric_Count;
         else
            Increment_Non_Alphanumeric_Count;
         end if;
      end loop;
      -- No exception handler here, exception propagates.
      Report.Failed ("No exception raised in child package subprogram");
   end Process_Text;

end Fb40a00.Cb40a01_0;

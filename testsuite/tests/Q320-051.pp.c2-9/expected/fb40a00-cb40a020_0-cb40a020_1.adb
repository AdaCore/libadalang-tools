procedure Fb40a00.Cb40a020_0.Cb40a020_1 (Text : in String) is
   Pos : Natural := Text'First - 1;
begin
   loop   -- Process string, raise exception upon completion.
      Pos := Pos + 1;
      if Pos > Text'Last then
         raise Completed_Text_Processing;
      elsif (Text (Pos) in 'A' .. 'Z') or
        (Text (Pos) in 'a' .. 'z') or
        (Text (Pos) in '0' .. '9')
      then
         Increment_Alphanumeric_Count;
      else
         Increment_Non_Alphanumeric_Count;
      end if;
   end loop;
   -- No exception handler here, exception propagates.
   Report.Failed ("No exception raised in child package subprogram");
end Fb40a00.Cb40a020_0.Cb40a020_1;

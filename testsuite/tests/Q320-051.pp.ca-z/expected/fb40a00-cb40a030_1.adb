     --=================================================================--

package body Fb40a00.Cb40a030_1 is

   procedure Process_Text (Text : in String) is
      Loop_Count : Integer := Text'Length + 1;
   begin
      for Pos in 1 .. Loop_Count loop          -- Process string, force the
         -- raise of Constraint_Error.
         if (Text (Pos) in 'a' .. 'z') or
           (Text (Pos) in 'A' .. 'Z') or
           (Text (Pos) in '0' .. '9')
         then
            Increment_Alphanumeric_Count;
         else
            Increment_Non_Alphanumeric_Count;
         end if;

      end loop;
      -- No exception handler here, exception propagates.
   end Process_Text;

end Fb40a00.Cb40a030_1;

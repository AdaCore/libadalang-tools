package Simple is

   procedure Test (Str : String) with
     Pre => (for all C of Str => C in '0' .. '9');

   procedure Test_2 (Str : String) with
     Pre => (if Str'Length = 1 then Str (Str'First) = 'a' else True);

end Simple;

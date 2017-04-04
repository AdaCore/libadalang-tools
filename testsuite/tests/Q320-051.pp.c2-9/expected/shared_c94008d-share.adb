separate (Shared_C94008d)
task body Share is
   Variable : Holder_Type;
begin
   loop
      select
         accept Set (Value : in Holder_Type) do
            Shared_C94008d.Set (Variable, Value);
         end Set;
      or
         accept Update (Value : in Value_Type) do
            Shared_C94008d.Update (Variable, Value);
         end Update;
      or
         accept Read (Value : out Holder_Type) do
            Value := Variable;
         end Read;
      or
         terminate;
      end select;
   end loop;
end Share;

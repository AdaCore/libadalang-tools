procedure A is
begin
   select when T => null; then abort null; end select;
   select when T => null; then abort end select;
end A;


procedure Main is
   procedure C_ Function
   with Convention => C,
        Import     => True,
        Link_Name  => "c_function";

begin
   C_Function;
end Main;

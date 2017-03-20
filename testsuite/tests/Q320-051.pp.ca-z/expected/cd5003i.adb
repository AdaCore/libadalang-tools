with Cd5003i_Pack3; use Cd5003i_Pack3;
with Report;        use Report;
pragma Elaborate (Report);
procedure Cd5003i is
   procedure Proc3 is new Proc2;
begin
   Proc3;
end Cd5003i;

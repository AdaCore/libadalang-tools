with Cd7202a_Sys;
with Report; use Report;
procedure Cd7202a is

   Int : Integer := 2;

   Bool : Boolean := (Int'Address in Cd7202a_Sys.My_Address);

begin
   Test
     ("CD7202A",
      "THE 'ADDRESS ATTRIBUTE CAN BE USED IN A" &
      " COMPILATION UNIT EVEN IF A WITH CLAUSE FOR " &
      "PACKAGE SYSTEM DOES NOT APPLY TO THE UNIT");

   if not Ident_Bool (Bool) then
      Failed ("ADDRESS ATTRIBUTE INCORRECT");
   end if;

   Result;
end Cd7202a;

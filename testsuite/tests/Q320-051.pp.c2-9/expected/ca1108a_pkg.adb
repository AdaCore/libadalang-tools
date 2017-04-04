package body Ca1108a_Pkg is

   procedure Sub (X, Y : in out Integer) is separate;

   procedure Proc is
      Y : Integer := 2;
   begin
      Y := Other_Pkg.I;
      if Y /= 4 then
         Failed
           ("OTHER_PKG VARIABLE NOT VISIBLE " & "IN PACKAGE BODY PROCEDURE");
      end if;
   end Proc;

   procedure Call_Subs (X, Y : in out Integer) is
   begin
      Sub (X, Y);
   end Call_Subs;

begin

   J := F (J);            -- J => J + 1.
   if J /= 3 then
      Failed ("OTHER_PKG FUNCTION NOT VISIBLE IN " & "PACKAGE BODY");
   end if;

end Ca1108a_Pkg;

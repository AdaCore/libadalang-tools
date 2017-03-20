with Later_Pkg;
pragma Elaborate (Later_Pkg);
package body Ca1108b_Pkg is

   procedure Sub (X, Y : in out Integer) is separate;

   procedure Proc is
      I, J : Integer;
   begin
      I := First_Pkg.F;
      if I /= 1 then
         Failed
           ("FIRST_PKG FUNCTION NOT VISIBLE IN " & "PACKAGE BODY PROCEDURE");
      end if;
      J := Later_Pkg.F;
      if J /= 3 then
         Failed
           ("LATER_PKG FUNCITON NOT VISIBLE IN " & "PACKAGE BODY PROCEDURE");
      end if;
   end Proc;

   procedure Call_Subs (X, Y : in out Integer) is
   begin
      Sub (X, Y);
   end Call_Subs;

begin

   I := First_Pkg.F;
   if I /= 1 then
      Failed ("FIRST_PKG FUNCTION NOT VISIBLE IN PACKAGE BODY");
   end if;
   J := Later_Pkg.F;
   if J /= 3 then
      Failed ("LATER_PKG FUNCTION NOT VISIBLE IN PACKAGE BODY");
   end if;

end Ca1108b_Pkg;

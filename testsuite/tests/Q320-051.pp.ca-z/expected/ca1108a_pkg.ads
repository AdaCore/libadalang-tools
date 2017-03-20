with Report, Other_Pkg;
use Report, Other_Pkg;
pragma Elaborate (Other_Pkg);
package Ca1108a_Pkg is

   J : Integer := 2;
   procedure Proc;
   procedure Call_Subs (X, Y : in out Integer);

end Ca1108a_Pkg;

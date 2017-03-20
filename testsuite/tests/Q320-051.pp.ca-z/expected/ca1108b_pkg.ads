with Report, First_Pkg;
use Report;
pragma Elaborate (First_Pkg);
package Ca1108b_Pkg is

   I, J : Integer;
   procedure Proc;
   procedure Call_Subs (X, Y : in out Integer);

end Ca1108b_Pkg;

with Report; use Report;
pragma Elaborate (Report);
package body Ca2011b1 is
   package Ca2011bx renames Ca2011b0;
   procedure P1 (X : T) is separate;
   procedure P2 (X : Ca2011bx.T) is separate;
end Ca2011b1;

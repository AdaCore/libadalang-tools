with Report; use Report;
pragma Elaborate (Report);
package body Header is

   procedure Wrong (Why : String) is
   begin
      Failed ("PACKAGE WITH " & Why & " NOT ELABORATED " & "CORRECTLY");
   end Wrong;

begin

   Test
     ("CA5004B",
      "PRAGMA ELABORATE IS ACCEPTED AND OBEYED " &
      "EVEN WHEN THE BODY OF THE UNIT NAMED IS " &
      "MISSING OR OBSOLETE");

end Header;

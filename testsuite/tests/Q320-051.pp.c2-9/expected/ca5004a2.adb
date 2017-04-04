with Report;   use Report;
with Ca5004a1; use Ca5004a1;
pragma Elaborate (Ca5004a1, Report);
package body Ca5004a2 is

   I : Integer := 1;

   procedure Require_Body is
   begin
      null;
   end Require_Body;
begin

   Test
     ("CA5004A",
      "APPLYING PRAGMA ELABORATE TO A PACKAGE " &
      "DECLARING A TASK OBJECT CAUSES IMPLICIT " &
      "BODY ELABORATION AND TASK ACTIVATION");

   select
      T.E (I);
      if I /= 4 then
         Failed ("TASK NOT EXECUTED PROPERLY");
      end if;
   or
      delay 10.0;
      Failed ("TASK NOT ACTIVATED AFTER 10 SECONDS");
   end select;

end Ca5004a2;

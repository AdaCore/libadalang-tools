
package body Assertions is

   pragma Assert (X
     and then Y
     and then Z);

begin
   if X
     and then Y
     and then Z
   then
      null;
   end if;

end Assertions;

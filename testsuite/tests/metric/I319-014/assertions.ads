package Assertions is

   pragma Elaborate_Body;

   X, Y, Z : Boolean := False;

   procedure P with
     Pre => X
       and then Y
       and then Z,
     Post => X
       and then Y
       and then Z;

end Assertions;

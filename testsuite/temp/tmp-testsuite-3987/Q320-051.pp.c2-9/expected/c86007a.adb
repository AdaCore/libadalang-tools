with C86007a_Pack;
with Report; use Report;
procedure C86007a is
begin
   Test
     ("C86007A",
      "CHECK THAT AN EXPANDED NAME FOR AN ENTITY " &
      "DECLARED IN THE VISIBLE PART OF A LIBRARY " &
      "PACKAGE CAN START WITH THE NAME ""STANDARD""");

   Standard.C86007a_Pack.Proc;

   Result;
end C86007a;

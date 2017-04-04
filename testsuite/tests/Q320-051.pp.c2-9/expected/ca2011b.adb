with Report; use Report;
with Ca2011b0, Ca2011b1;
procedure Ca2011b is

   package P1 is
      subtype T is Integer range -100 .. 100;
   end P1;
   use P1;

   function F1 return P1.T;
   function F2 return T;

   package P2 renames P1;

   function F1 return T is separate;
   function F2 return P2.T is separate;

begin
   Test
     ("CA2011B",
      "CHECK THAT FOR A SUBPROGRAM DECLARATION-STUB-" &
      "BODY TRIPLE, THE DECLARATION-STUB AND STUB-" &
      "BODY SPECIFICATIONS CAN CONFORM, BUT THE " &
      "DECLARATON-BODY SPECIFICATIONS NEED NOT");

   if F1 /= Ident_Int (100) then
      Failed ("INCORRECT RETURN VALUE FROM FUNCTION 1");
   end if;

   if F2 /= Ident_Int (-100) then
      Failed ("INCORRECT RETURN VALUE FROM FUNCTION 2");
   end if;

   Ca2011b1.P1 (3);
   if Ca2011b0.I /= Ident_Int (3) then
      Failed ("INCORRECT RETURN VALUE FROM PROCEDURE 1");
   end if;

   Ca2011b1.P2 (4);
   if Ca2011b0.I /= Ident_Int (4) then
      Failed ("INCORRECT RETURN VALUE FROM PROCEDURE 2");
   end if;

   Result;
end Ca2011b;

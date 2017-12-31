with Report; use Report;
with Cc3019a_Queues;
procedure Cc3019a is

   subtype Str6 is String (1 .. 6);

   type Str6_Arr is array (1 .. 3) of Str6;
   Str6_Vals     : Str6_Arr := ("111111", "222222", Ident_Str ("333333"));
   Cur_Str_Index : Natural  := 1;

   type Int_Arr is array (1 .. 3) of Integer;
   Int_Vals      : Int_Arr := (-1, 3, Ident_Int (3));
   Cur_Int_Index : Natural := 1;

-- THIS PROCEDURE IS CALLED ONCE FOR EACH ELEMENT OF THE QUEUE
--
   procedure Check_Str (Val : Str6) is
   begin
      if Val /= Str6_Vals (Cur_Str_Index) then
         Failed
           ("STR6 ITERATOR FOR INDEX =" & Integer'Image (Cur_Str_Index) &
            " WITH VALUE " & """" & Val & """");
      end if;
      Cur_Str_Index := Cur_Str_Index + 1;
   exception
      when Constraint_Error =>
         Failed ("STR6 - CONSTRAINT_ERROR RAISED");
      when others =>
         Failed ("STR6 - UNEXPECTED EXCEPTION");
   end Check_Str;

   procedure Check_Int (Val : Integer) is
   begin
      if Val /= Int_Vals (Cur_Int_Index) then
         Failed
           ("INTEGER ITERATOR FOR INDEX =" & Integer'Image (Cur_Int_Index) &
            " WITH VALUE " & """" & Integer'Image (Val) & """");
      end if;
      Cur_Int_Index := Cur_Int_Index + 1;
   exception
      when Constraint_Error =>
         Failed ("INTEGER - CONSTRAINT_ERROR RAISED");
      when others =>
         Failed ("INTEGER - UNEXPECTED EXCEPTION");
   end Check_Int;

   package Str6_Queue is new Cc3019a_Queues (Str6);
   use Str6_Queue;

   package Int_Queue is new Cc3019a_Queues (Integer);
   use Int_Queue;

begin

   Test ("CC3019A", "CHECK NESTED GENERICS - ITERATORS");

   declare
      Q1 : Str6_Queue.Queue_Type;

      procedure Chk_Str is new Str6_Queue.Iterator (Check_Str);

   begin

      Add (Q1, "111111");
      Add (Q1, "222222");
      Add (Q1, "333333");

      Cur_Str_Index := 1;
      Chk_Str (Q1);

   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION - Q1");
   end;

-- REPEAT FOR INTEGERS

   declare
      Q2 : Int_Queue.Queue_Type;

      procedure Chk_Int is new Int_Queue.Iterator (Check_Int);

   begin

      Add (Q2, -1);
      Add (Q2, 3);
      Add (Q2, 3);

      Cur_Int_Index := 1;
      Chk_Int (Q2);

   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION - Q2");
   end;

   Result;

end Cc3019a;

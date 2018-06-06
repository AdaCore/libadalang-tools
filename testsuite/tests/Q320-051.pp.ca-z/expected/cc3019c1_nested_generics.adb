package body Cc3019c1_Nested_Generics is

   procedure Copy
     (Source      : in out Nested_Generics_Type;
      Destination : in out Nested_Generics_Type)
   is

   begin  -- COPY

      Assign (Source => Source.First, Destination => Destination.First);

      Destination.Second := Source.Second;

   end Copy;

   procedure Set_Element
     (For_This_Ngt_Object : in out Nested_Generics_Type;
      To_This_Element     : in out Element)
   is

   begin  -- SET_ELEMENT

      Assign
        (Source => To_This_Element, Destination => For_This_Ngt_Object.First);

   end Set_Element;

   procedure Set_Number
     (For_This_Ngt_Object : in out Nested_Generics_Type;
      To_This_Number      : in     Natural)
   is

   begin  -- SET_NUMBER

      For_This_Ngt_Object.Second := To_This_Number;

   end Set_Number;

   function "="
     (Left : in Nested_Generics_Type; Right : in Nested_Generics_Type)
      return Boolean
   is

   begin  -- "="

      if (Left.First = Right.First) and (Left.Second = Right.Second) then
         return True;
      else
         return False;
      end if;

   end "=";

   function Element_Of
     (This_Ngt_Object : in Nested_Generics_Type) return Element
   is

   begin  -- ELEMENT_OF

      return This_Ngt_Object.First;

   end Element_Of;

   function Number_Of
     (This_Ngt_Object : in Nested_Generics_Type) return Natural
   is

   begin  -- NUMBER_OF

      return This_Ngt_Object.Second;

   end Number_Of;

   package body Generic_Task is

      task body Protected_Area is

         Local_Store : Element;

      begin  -- PROTECTED_AREA

         loop
            select
               accept Store (Item : in out Element) do
                  Assign (Source => Item, Destination => Local_Store);
               end Store;
            or
               accept Get (Item : in out Element) do
                  Assign (Source => Local_Store, Destination => Item);
               end Get;
            or
               terminate;
            end select;
         end loop;

      end Protected_Area;

   end Generic_Task;

   package body Stack_Class is

      procedure Push
        (This_Element : in out Element; On_To_This_Stack : in out Stack)
      is

      begin  -- PUSH

         New_List_Class.Add
           (This_Element => This_Element,
            To_This_List => New_List_Class.List (On_To_This_Stack));

      exception

         when New_List_Class.Overflow =>
            raise Overflow;

      end Push;

      procedure Pop
        (This_Element : in out Element; Off_This_Stack : in out Stack)
      is

      begin  -- POP

         New_List_Class.Delete
           (This_Element   => This_Element,
            From_This_List => New_List_Class.List (Off_This_Stack));

      exception

         when New_List_Class.Underflow =>
            raise Underflow;

      end Pop;

      procedure Copy (This_Stack : in out Stack; To_This_Stack : in out Stack)
      is

      begin  -- COPY

         New_List_Class.Copy
           (This_List    => New_List_Class.List (This_Stack),
            To_This_List => New_List_Class.List (To_This_Stack));

      end Copy;

      procedure Clear (This_Stack : in out Stack) is

      begin  -- CLEAR

         New_List_Class.Clear (New_List_Class.List (This_Stack));

      end Clear;

      procedure Iterate (Over_This_Stack : in Stack) is

         procedure Stack_Iterate is new New_List_Class.Iterate
           (Process => Process);

      begin  -- ITERATE

         Stack_Iterate (New_List_Class.List (Over_This_Stack));

      end Iterate;

      function Number_Of_Elements (On_This_Stack : in Stack) return Natural is

      begin  -- NUMBER_OF_ELEMENTS

         return New_List_Class.Number_Of_Elements
             (In_This_List => New_List_Class.List (On_This_Stack));

      end Number_Of_Elements;

      function "=" (Left : in Stack; Right : in Stack) return Boolean is

      begin  -- "="

         return New_List_Class."="
             (Left  => New_List_Class.List (Left),
              Right => New_List_Class.List (Right));

      end "=";

   end Stack_Class;

end Cc3019c1_Nested_Generics;

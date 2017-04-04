package body Cc3019b1_Stack_Class is

   procedure Push
     (This_Element     : in out Element;
      On_To_This_Stack : in out Stack)
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
     (This_Element   : in out Element;
      Off_This_Stack : in out Stack)
   is

   begin  -- POP

      New_List_Class.Delete
        (This_Element   => This_Element,
         From_This_List => New_List_Class.List (Off_This_Stack));

   exception

      when New_List_Class.Underflow =>
         raise Underflow;

   end Pop;

   procedure Copy (This_Stack : in out Stack; To_This_Stack : in out Stack) is

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

end Cc3019b1_Stack_Class;

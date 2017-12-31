package body Cc3019c0_List_Class is

   procedure Add (This_Element : in out Element; To_This_List : in out List) is

   begin  -- ADD

      if To_This_List.Length >= 10 then
         raise Overflow;
      else
         To_This_List.Length := To_This_List.Length + 1;
         Assign
           (Source      => This_Element,
            Destination => To_This_List.Actual_List (To_This_List.Length));
      end if;

   end Add;

   procedure Delete (This_Element : in out Element;
      From_This_List              : in out List)
   is

   begin  -- DELETE

      if From_This_List.Length <= 0 then
         raise Underflow;
      else
         Assign
           (Source      => From_This_List.Actual_List (From_This_List.Length),
            Destination => This_Element);
         From_This_List.Length := From_This_List.Length - 1;
      end if;

   end Delete;

   procedure Copy (This_List : in out List; To_This_List : in out List) is

   begin  -- COPY

      To_This_List.Length := This_List.Length;
      for Index in To_This_List.Actual_List'Range loop
         Assign
           (Source      => This_List.Actual_List (Index),
            Destination => To_This_List.Actual_List (Index));
      end loop;

   end Copy;

   procedure Clear (This_List : in out List) is

   begin  -- CLEAR

      This_List.Length := 0;

   end Clear;

   procedure Iterate (Over_This_List : in List) is

      Continue : Boolean := True;
      Finished : Natural := 0;

   begin  -- ITERATE

      while (Continue = True) and (Finished < Over_This_List.Length) loop
         Finished := Finished + 1;
         Process
           (This_Element => Over_This_List.Actual_List (Finished),
            Continue     => Continue);
      end loop;

   end Iterate;

   function Number_Of_Elements (In_This_List : in List) return Natural is

   begin  -- NUMBER_OF_ELEMENTS

      return In_This_List.Length;

   end Number_Of_Elements;

   function "=" (Left : in List; Right : in List) return Boolean is

      Result : Boolean := True;
      Index  : Natural := 0;

   begin  -- "="

      if Left.Length /= Right.Length then
         Result := False;
      else
         while (Index < Left.Length) and Result loop
            Index := Index + 1;
            if Left.Actual_List (Index) /= Right.Actual_List (Index) then
               Result := False;
            end if;
         end loop;
      end if;

      return Result;

   end "=";

end Cc3019c0_List_Class;

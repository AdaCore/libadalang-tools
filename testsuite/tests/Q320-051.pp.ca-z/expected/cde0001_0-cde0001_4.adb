--==================================================================--

with Cde0001_0.Cde0001_3;            -- private sibling.

pragma Elaborate (Cde0001_0.Cde0001_3);

package body Cde0001_0.Cde0001_4 is

   Obj1 : Private_Type            := 2;
   Obj2 : Limited_Private         := Bee;
   Obj3 : Private_Subtype         := 3;
   Obj4 : Limited_Private_Subtype := Sea;
   Obj5 : New_Tagtype             := (1, 5);

   -- Instantiate the generic package declared in the visible part of
   -- the parent.

   package Formal_Obj_Pck is new Cde0001_1 (Obj1, Obj2, Obj3, Obj4, Obj5);

   ---------------------------------------------------
   function Verify_Objects return Boolean is
      Result : Boolean := False;
   begin
      if Obj1 = 1 and
        Obj2 = Dee and
        Obj3 = 10 and
        Obj4 = Eh and
        Obj5.C1 = 2 and
        Obj5.C2 = 10
      then
         Result := True;
      end if;

      return Result;

   end Verify_Objects;

   ---------------------------------------------------
   function Verify_Arrays return Boolean is
      Result : Boolean := False;
   begin
      if Cde0001_0.Cde0001_3.Arr01_Obj (1) = 9 and
        Cde0001_0.Cde0001_3.Arr01_Obj (2) = 2 and
        Cde0001_0.Cde0001_3.Arr02_Obj (1).C1 = Bee and
        Cde0001_0.Cde0001_3.Arr02_Obj (2).C1 = Sea
      then
         Result := True;
      end if;

      return Result;

   end Verify_Arrays;

   ---------------------------------------------------
   function Verify_Access return Boolean is
      Result : Boolean := False;
   begin
      if Cde0001_0.Cde0001_3.Acc01_Obj.C1 = 10 and
        Cde0001_0.Cde0001_3.Acc02_Obj (1) = Eh and
        Cde0001_0.Cde0001_3.Acc02_Obj (2) = Dee
      then
         Result := True;
      end if;

      return Result;

   end Verify_Access;

begin

   Formal_Obj_Pck.Assign_Objects;

   Cde0001_0.Cde0001_3.Formal_Types_Pck.Assign_Arrays
     (Cde0001_0.Cde0001_3.Arr01_Obj,
      Cde0001_0.Cde0001_3.Arr02_Obj);
   Cde0001_0.Cde0001_3.Formal_Types_Pck.Assign_Access
     (Cde0001_0.Cde0001_3.Acc01_Obj,
      Cde0001_0.Cde0001_3.Acc02_Obj);

end Cde0001_0.Cde0001_4;

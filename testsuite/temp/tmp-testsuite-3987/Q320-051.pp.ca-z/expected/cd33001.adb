-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- there is no package body CD33001_0

------------------------------------------------------------------- CD33001

with Report;
with System.Storage_Elements;
with Cd33001_0;
procedure Cd33001 is

   use type System.Storage_Elements.Storage_Offset;

   A_Half : Cd33001_0.Half_Stuff (0 .. 15);

   A_Word : Cd33001_0.Word_Stuff (0 .. 15);

   procedure Unexpected (Message : String; Wanted, Got : Integer) is
   begin
      Report.Failed
        (Message &
         " Wanted:" &
         Integer'Image (Wanted) &
         " Got:" &
         Integer'Image (Got));
   end Unexpected;

begin  -- Main test procedure.

   Report.Test
     ("CD33001",
      "Check that Component_Sizes that are factor of " &
      "the word size are supported.  Check that for " &
      "such Component_Sizes arrays contain no gaps " &
      "between components");

   if A_Half'Size /= A_Half'Component_Size * 16 then
      Unexpected
        ("Half word Size",
         Cd33001_0.Half_Stuff'Component_Size * 16,
         A_Half'Size);
   end if;

   if A_Word (1)'Size /= System.Word_Size then
      Unexpected ("Word Size", System.Word_Size, A_Word (1)'Size);
   end if;

   Report.Result;

end Cd33001;

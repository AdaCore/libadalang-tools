     --===================================================================--

with Cc54004_0;
generic
   type Element_Type is abstract new Cc54004_0.Alert with private;
   type Element_Ptr is access Element_Type'Class;
package Cc54004_2 is

   type Stack_Type is private;

   procedure Push (Stack : in out Stack_Type; Elem_Ptr : in Element_Ptr);

   procedure Pop (Stack : in out Stack_Type; Elem_Ptr : out Element_Ptr);

   procedure Process_Stack (Stack : in out Stack_Type);

   -- ... Other operations.

private

   subtype Index is Positive range 1 .. 5;
   type Stack_Type is array (Index) of Element_Ptr;

   Top : Index := 1;

end Cc54004_2;

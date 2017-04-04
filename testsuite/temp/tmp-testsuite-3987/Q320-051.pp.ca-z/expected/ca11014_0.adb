--==================================================================--

package body Ca11014_0 is

   function End_Of_List (L : List_Type) return Boolean is
   begin
      return (L.Current = null);
   end End_Of_List;
   -------------------------------------------------------
   procedure Reset (L : in out List_Type) is
   begin
      L.Current := L.First;                 -- Set "current" pointer to first
   end Reset;                               -- list element.

end Ca11014_0;

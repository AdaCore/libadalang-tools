-----------------------------------------------------------------------------

with Report;
with Cxe4005_Part_A2;
with Cxe4005_Part_B;
with Cxe4005_Normal;
with Cxe4005_Remote_Types;
package body Cxe4005_Part_A1 is
   Root_Obj   : aliased Cxe4005_Common.Root_Tagged_Type;
   Rt_Obj     : aliased Cxe4005_Remote_Types.Rt_Tagged_Type;
   Normal_Obj : aliased Cxe4005_Normal.Cant_Use_In_Remote_Call;

   ---------  partition termination coordination ----------
   -- use a task to prevent the partition from completing its execution
   -- until the main procedure in partition B tells it to quit.

   task Wait_For_Quit is
      entry Can_Quit;
      entry Quit;
   end Wait_For_Quit;

   task body Wait_For_Quit is
   begin
      accept Can_Quit; -- Called once we've called Report.Test. (Else we might
      -- call Report.Result before Report.Test.)
      accept Quit;
      Report.Result;
   end Wait_For_Quit;

   procedure Can_Quit is
   begin
      Wait_For_Quit.Can_Quit;
   end Can_Quit;

   procedure Quit is
   begin
      Wait_For_Quit.Quit;
   end Quit;

   function Get_Racwt
     (Which_Type : Type_Selection) return Cxe4005_Part_A1.Racwt
   is
   begin
      case Which_Type is
         when Common_Spec =>
            return Root_Obj'Access;
         when Rt_Spec =>
            return Rt_Obj'Access;
         when B_Body =>
            return null;
         when Normal_Spec =>
            return Normal_Obj'Access;
      end case;
   end Get_Racwt;

   procedure Takes_Class_Wide (X : Cxe4005_Common.Open_Tagged_Type'Class) is
   begin
      Cxe4005_Common.Open_Op (X);
   end Takes_Class_Wide;

   package Nested is
      type Body_Open_Tagged_Type is new Cxe4005_Common.Open_Tagged_Type with
      null record;
   end Nested;

   function Return_Open_Tagged_Type_Class
     return Cxe4005_Common.Open_Tagged_Type'Class
   is
      -- Return an object of a type not visible to the B partition.
      Obj : Nested.Body_Open_Tagged_Type;
   begin
      return Obj;
   end Return_Open_Tagged_Type_Class;

begin
   Set_Serial_Number (Root_Tagged_Type (Root_Obj)'Access, 101);
   Set_Serial_Number (Root_Tagged_Type (Rt_Obj)'Access, 106);
   -- no 107 object
   Set_Serial_Number (Root_Tagged_Type (Normal_Obj)'Access, 108);
end Cxe4005_Part_A1;

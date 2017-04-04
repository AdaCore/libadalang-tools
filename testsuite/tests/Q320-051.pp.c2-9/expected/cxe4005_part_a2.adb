-----------------------------------------------------------------------------

with Report;
with Cxe4005_Part_B;
with Cxe4005_Normal;
with Cxe4005_Remote_Types;
package body Cxe4005_Part_A2 is
   Root_Obj   : aliased Cxe4005_Common.Root_Tagged_Type;
   Rt_Obj     : aliased Cxe4005_Remote_Types.Rt_Tagged_Type;
   Normal_Obj : aliased Cxe4005_Normal.Cant_Use_In_Remote_Call;

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

begin
   Set_Serial_Number (Root_Tagged_Type (Root_Obj)'Access, 201);
   Set_Serial_Number (Root_Tagged_Type (Rt_Obj)'Access, 206);
   -- no 207 object
   Set_Serial_Number (Root_Tagged_Type (Normal_Obj)'Access, 208);
end Cxe4005_Part_A2;

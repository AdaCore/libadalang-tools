with Ada.Finalization;
with Cxc7006_1;
package Cxc7006_2 is
   type Fin_Type is new Ada.Finalization.Controlled with record
      Raise_Exc : Boolean := False;
      Fin_Id    : Integer := -1;
   end record;

   Fin_Exception : exception;

   overriding procedure Finalize (Obj : in out Fin_Type);

   procedure Clear_Fin_Array;

   procedure Test_Finalized
     (Id        : in     Integer;
      Finalized :    out Boolean;
      Err_Msg   :    out Cxc7006_1.String_Acc);

end Cxc7006_2;

package body C760014_0 is

   procedure Initialize (Obj : in out Test_Type) is
   begin
      Obj.Tc_Data.Init_Count := Obj.Tc_Data.Init_Count + 1;
      Obj.Dummy              := 1;
   end Initialize;

   procedure Finalize (Obj : in out Test_Type) is
   begin
      Obj.Tc_Data.Fin_Count := Obj.Tc_Data.Fin_Count + 1;
   end Finalize;

end C760014_0;

package body Qux is
   task body Dummy_Task_Type is
   begin
      accept Dummy_A (Dummy_Boolean : Boolean);
      accept Dummy_B (Dummy_Boolean : Boolean);
      accept Dummy_C (Dummy_Integer : Integer);
      accept Dummy_D;
      accept Dummy_E;
   end Dummy_Task_Type;
   procedure Dummy_F (Dummy_Boolean : Boolean) is null;
end Qux;

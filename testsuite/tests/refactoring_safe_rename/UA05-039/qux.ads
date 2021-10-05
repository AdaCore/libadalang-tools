package Qux is
   task type Dummy_Task_Type is
      entry Dummy_A (Dummy_Boolean : Boolean);
      entry Dummy_B (Dummy_Boolean : Boolean);
      entry Dummy_C (Dummy_Integer : Integer);
      entry Dummy_D;
      entry Dummy_E;
   end Dummy_Task_Type;
   procedure Dummy_F (Dummy_Boolean : Boolean);
end Qux;

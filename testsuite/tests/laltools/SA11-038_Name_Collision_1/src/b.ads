package B is

   type C is (L1, L2, L3);
   
   D1, D2 : C := L1;
   
   procedure E;
   
   task F;
   
   task type G;
   
   package I is
      type I_Type is (I1, I2, I3);
      procedure Print_I (I_Spec : I_Type);
   end I;
   
   package J is
      type J_Type is (J1, J2, J3);
      procedure Print_J (J_Spec : J_Type);
   end J;
   
private
   
   type C_Private is (CP1, CP2, CP3);

end B;

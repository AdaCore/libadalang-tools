package body My_Package is

   procedure My_Procedure is
      Start_Index : constant Integer := 1;
      End_Index   : constant Integer := 10;

   begin
      J_Loop :
      for J in Start_Index .. End_Index loop
         K_Loop :
         for K in Start_Index .. End_Index loop
            L_Loop :
            for L in Start_Index .. End_Index loop
               exit J_Loop;
            end loop L_Loop;
         end loop K_Loop;
      end loop J_Loop;
   end My_Procedure;

end My_Package;

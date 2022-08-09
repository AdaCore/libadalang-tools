package body A is

   procedure Subprogram is
      M : Map;
   begin
      Subprogram;
      Private_Subprogram_1;
      Private_Subprogram_2;
   end Subprogram;

   procedure Private_Subprogram_1 is
      M : Map;
   begin
      Subprogram;
      Private_Subprogram_1;
      Private_Subprogram_2;
   end Private_Subprogram_1;

   package body B is

      procedure Subprogram is
      begin
         null;
      end Subprogram;

      procedure Private_Subprogram_1 is
      begin
         null;
      end Private_Subprogram_1;

      package body C is

         procedure Subprogram is
         begin
            null;
         end Subprogram;

         procedure Private_Subprogram_1 is
         begin
            null;
         end Private_Subprogram_1;

         package body D is

            procedure Subprogram is
               M : Map;
            begin
               Subprogram;
               Private_Subprogram_1;
               Private_Subprogram_2;
            end Subprogram;

            procedure Private_Subprogram_1 is
               M : Map;
            begin
               Subprogram;
               Private_Subprogram_1;
               Private_Subprogram_2;
            end Private_Subprogram_1;

            procedure Private_Subprogram_2 is
               M : Map;
            begin
               Subprogram;
               Private_Subprogram_1;
               Private_Subprogram_2;
            end Private_Subprogram_2;

         end D;

         procedure Private_Subprogram_2 is
         begin
            null;
         end Private_Subprogram_2;

      end C;

      procedure Private_Subprogram_2 is
      begin
         null;
      end Private_Subprogram_2;

   end B;

   procedure Private_Subprogram_2 is
      M : Map;
   begin
      Subprogram;
      Private_Subprogram_1;
      Private_Subprogram_2;
   end Private_Subprogram_2;

end A;

package A is

   procedure Subprogram;

   package B is

      procedure Subprogram;

      package C is

         procedure Subprogram;

         package D is

            procedure Subprogram;

         private

            procedure Private_Subprogram_1;

            procedure Private_Subprogram_2;

         end D;

      private

         procedure Private_Subprogram_1;

         procedure Private_Subprogram_2;

      end C;

   private

      procedure Private_Subprogram_1;

      procedure Private_Subprogram_2;

   end B;

private

   procedure Private_Subprogram_1;

   procedure Private_Subprogram_2;

end A;

package body A is

   procedure Subprogram is
   begin
      Private_Subprogram_1;
      Private_Subprogram_2;
      B.Subprogram;
      B.C.Subprogram;
      B.C.D.Subprogram;
      Subprogram;
      null;
   end Subprogram;

   procedure Private_Subprogram_1 is
   begin
      Subprogram;
      Private_Subprogram_2;
      B.Subprogram;
      B.C.Subprogram;
      B.C.D.Subprogram;
      Private_Subprogram_1;
      null;
   end Private_Subprogram_1;

   package body B is

      procedure Subprogram is
      begin
         A.Subprogram;
         A.Private_Subprogram_1;
         A.Private_Subprogram_2;
         Private_Subprogram_1;
         Private_Subprogram_2;
         C.Subprogram;
         C.D.Subprogram;
         Subprogram;
         null;
      end Subprogram;

      procedure Private_Subprogram_1 is
      begin
         A.Subprogram;
         A.Private_Subprogram_1;
         A.Private_Subprogram_2;
         Subprogram;
         Private_Subprogram_2;
         C.Subprogram;
         C.D.Subprogram;
         Private_Subprogram_1;
         null;
      end Private_Subprogram_1;

      package body C is

         procedure Subprogram is
         begin
            A.Subprogram;
            A.Private_Subprogram_1;
            A.Private_Subprogram_2;
            A.B.Subprogram;
            A.B.Private_Subprogram_1;
            A.B.Private_Subprogram_2;
            Private_Subprogram_1;
            Private_Subprogram_2;
            D.Subprogram;
            Subprogram;
            null;
         end Subprogram;

         procedure Private_Subprogram_1 is
         begin
            A.Subprogram;
            A.Private_Subprogram_1;
            A.Private_Subprogram_2;
            A.B.Subprogram;
            A.B.Private_Subprogram_1;
            A.B.Private_Subprogram_2;
            Subprogram;
            Private_Subprogram_2;
            D.Subprogram;
            Private_Subprogram_1;
            null;
         end Private_Subprogram_1;

         package body D is

            procedure Subprogram is
            begin
               A.Subprogram;
               A.Private_Subprogram_1;
               A.Private_Subprogram_2;
               A.B.Subprogram;
               A.B.Private_Subprogram_1;
               A.B.Private_Subprogram_2;
               A.B.C.Subprogram;
               A.B.C.Private_Subprogram_1;
               A.B.C.Private_Subprogram_2;
               Private_Subprogram_1;
               Private_Subprogram_2;
               Subprogram;
               null;
            end Subprogram;

            procedure Private_Subprogram_1 is
            begin
               A.Subprogram;
               A.Private_Subprogram_1;
               A.Private_Subprogram_2;
               A.B.Subprogram;
               A.B.Private_Subprogram_1;
               A.B.Private_Subprogram_2;
               A.B.C.Subprogram;
               A.B.C.Private_Subprogram_1;
               A.B.C.Private_Subprogram_2;
               Subprogram;
               Private_Subprogram_2;
               Private_Subprogram_1;
               null;
            end Private_Subprogram_1;

            procedure Private_Subprogram_2 is
            begin
               A.Subprogram;
               A.Private_Subprogram_1;
               A.Private_Subprogram_2;
               A.B.Subprogram;
               A.B.Private_Subprogram_1;
               A.B.Private_Subprogram_2;
               A.B.C.Subprogram;
               A.B.C.Private_Subprogram_1;
               A.B.C.Private_Subprogram_2;
               Subprogram;
               Private_Subprogram_1;
               Private_Subprogram_2;
               null;
            end Private_Subprogram_2;

         end D;

         procedure Private_Subprogram_2 is
         begin
            A.Subprogram;
            A.Private_Subprogram_1;
            A.Private_Subprogram_2;
            A.B.Subprogram;
            A.B.Private_Subprogram_1;
            A.B.Private_Subprogram_2;
            Subprogram;
            Private_Subprogram_1;
            D.Subprogram;
            Private_Subprogram_2;
            null;
         end Private_Subprogram_2;

      end C;

      procedure Private_Subprogram_2 is
      begin
         A.Subprogram;
         A.Private_Subprogram_1;
         A.Private_Subprogram_2;
         Subprogram;
         Private_Subprogram_1;
         C.Subprogram;
         C.D.Subprogram;
         Private_Subprogram_2;
         null;
      end Private_Subprogram_2;

   end B;

   procedure Private_Subprogram_2 is
   begin
      Subprogram;
      Private_Subprogram_1;
      B.Subprogram;
      B.C.Subprogram;
      B.C.D.Subprogram;
      Private_Subprogram_2;
      null;
   end Private_Subprogram_2;

end A;

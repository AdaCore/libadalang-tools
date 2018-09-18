package P is

   package Empty is
   end Empty;

   package Types is
      type My_Type is range 1 .. 10;
   end Types;

   package Incomplete_Types is
   private
      type Stt_Incomplete;
   end Incomplete_Types;

   package Private_Procedure is
   private
      procedure Q;
   end Private_Procedure;

end P;

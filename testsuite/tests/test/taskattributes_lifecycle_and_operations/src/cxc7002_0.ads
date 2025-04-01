with Ada.Task_Identification;

package CXC7002_0 is
   type Small_Integer is range -7 .. 7;
   type Int_Array is array (1..3) of Small_Integer;
   pragma Pack (Int_Array);

   Countdown : Int_Array := (3,2,1);

   task type Lib_Tasks is
      entry Get_Id (Id : out Ada.Task_Identification.Task_Id);
      entry Ok_To_Terminate;
   end Lib_Tasks;

   T1 : Lib_Tasks;
   T2 : Lib_Tasks;
end CXC7002_0;

     --==================================================================--

package body C390007_0 is

   procedure Classwide_Proc (X : in out Root_Type'Class) is
   begin
      Inner_Proc (X);
   end Classwide_Proc;

end C390007_0;

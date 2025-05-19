with System.Tasking;

package Pkg is
   function Do_Stuff
     (X : System.Tasking.Accept_Alternative)
      return System.Tasking.Accept_Alternative
   is (X);
end Pkg;

with System;
with System.Multiprocessors;
with System.Storage_Elements;
package P is

   task type TT
     (Base_Priority : System.Priority;
      Storage_Size  : System.Storage_Elements.Storage_Offset;
      Cpu           : System.Multiprocessors.Cpu_Range)
     with Priority     => Base_Priority,
          Storage_Size => Storage_Size,
          Cpu          => Cpu
   is
      entry Start (N : Integer);
   end TT;

   function F return Integer is (1);

end P;

--!pp off







package pp_off_test is

   X : constant Integer := 0;

   --  the POK method
   function POK
     (V : Float)
      return Boolean;


   --  the P method
   function P
     (V1 : Float;
      V2 : Float)
      return Boolean;


   --  the Q procedure
   procedure Q
     (V : Float);

end pp_off_test;

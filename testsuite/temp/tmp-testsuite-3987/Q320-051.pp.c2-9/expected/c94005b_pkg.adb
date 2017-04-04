package body C94005b_Pkg is

   task body Tt is
      Local : Integer;
   begin
      accept E (I : Integer) do
         Local := I;
      end E;
      delay 60.0;    -- SINCE THE PARENT UNIT HAS HIGHER PRIORITY
      -- AT THIS POINT, IT WILL RECEIVE CONTROL AND
      -- TERMINATE IF THE ERROR IS PRESENT.
      Global := Local;
   end Tt;

end C94005b_Pkg;

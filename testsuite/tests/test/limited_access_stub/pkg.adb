package body Pkg is

   procedure Get
     (S : access My_Stream'Class; Res : out A_String) is
   begin
      Res := "1234567890";
   end Get;

end Pkg;

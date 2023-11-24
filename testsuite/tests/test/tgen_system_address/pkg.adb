package body Pkg is

   -------------
   -- Setvbuf --
   -------------

   function Setvbuf
     (Buffer : Interfaces.C_Streams.Chars;
      Mode   : Interfaces.C_Streams.Int;
      Size   : Interfaces.C_Streams.Size_T)
      return Interfaces.C_Streams.Int
   is
   begin
      return 1;
   end Setvbuf;

end Pkg;

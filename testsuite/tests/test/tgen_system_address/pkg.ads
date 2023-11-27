with Interfaces.C_Streams;

package Pkg is

   function Setvbuf
     (Buffer : Interfaces.C_Streams.Chars;
      Mode   : Interfaces.C_Streams.Int;
      Size   : Interfaces.C_Streams.Size_T)
      return Interfaces.C_Streams.Int;

end Pkg;

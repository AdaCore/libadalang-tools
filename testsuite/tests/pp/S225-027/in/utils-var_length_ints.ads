with Utils.Vectors;

package Utils.Var_Length_Ints is

   --  This package provides a space-efficient encoding of nonnegative
   --  integers. The encoding is variable length, so each integer is encoded
   --  as a sequence of one or more Octets.

   --  Note that Octet should be "limited private", but annoying Ada
   --  restrictions prevent that.

   --  Octet should be a signed integer type; we have no use for modular
   --  arithmetic here. However, that seems to make gnatpp extremely
   --  slow, at least in dev mode (9 times slower in one case). Didn't
   --  try prod mode.

--   type Octet is range 0 .. 2**8 - 1;
   type Octet is mod 2**8;
   type Octet_Index is new Positive;
   type Octet_Array is array (Octet_Index range <>) of Octet;
   for Octet_Array'Component_Size use 8; -- needed if Octet is signed

   package Octet_Vectors is new Utils.Vectors (Octet_Index, Octet,
      Octet_Array);
   subtype Octet_Vector is Octet_Vectors.Vector;
   use Octet_Vectors;

   generic
      type Int is range <>;
   package Encodings is
      pragma Assert (Int'First >= 0);
      --  So we don't have to deal with negative numbers

      procedure Encode (V : in out Octet_Vector; X : Int);
      --  Appends the encoding of X onto V.

      function Decode (V : Octet_Vector; Index : Octet_Index) return Int;
      function Decode (A : Octet_Array; Index : Octet_Index) return Int;
   --  Decodes the Int starting at V(Index).
   end Encodings;

   function Next (V : Octet_Vector; Index : Octet_Index) return Octet_Index;
   function Next (A : Octet_Array; Index : Octet_Index) return Octet_Index;
   procedure Next (V : Octet_Vector; Index : in out Octet_Index);
   procedure Next (A : Octet_Array; Index : in out Octet_Index);
   --  Get index of next encoded integer

   function Prev (V : Octet_Vector; Index : Octet_Index) return Octet_Index;
   function Prev (A : Octet_Array; Index : Octet_Index) return Octet_Index;
   procedure Prev (V : Octet_Vector; Index : in out Octet_Index);
   procedure Prev (A : Octet_Array; Index : in out Octet_Index);
--  Get index of previous encoded integer

end Utils.Var_Length_Ints;

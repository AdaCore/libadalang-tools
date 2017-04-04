     --==================================================================--

with F3a2a00;
generic
   type Fd is new F3a2a00.Array_Type;
   Fobj : in F3a2a00.Tagged_Type;
package C3a2a02_2 is
   type Gaf is access all Fd;
   type Gao is access constant F3a2a00.Tagged_Type;
   Xg    : aliased Fd;
   Ptrf  : Gaf;
   Index : Integer := Fd'First;

   procedure Dummy; -- Needed to allow package body.
end C3a2a02_2;

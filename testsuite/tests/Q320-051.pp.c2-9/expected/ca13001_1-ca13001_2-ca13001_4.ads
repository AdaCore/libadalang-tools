-- No bodies required for CA13001_1.CA13001_2.CA13001_3.

     --==================================================================--

-- Context clauses required for visibility needed by a separate subunit.

with Ca13001_0; use Ca13001_0;

-- Public grandchild of a private parent.

package Ca13001_1.Ca13001_2.Ca13001_4 is

   type Transit is record
      Available : Boolean := False;
   end record;
   type Keys_Array is array (Transportation) of Transit;
   Fuel : array (Transportation) of Boolean := (others => True);

   protected Family_Transportation is

      procedure Get_Vehicle (Who : in Family; Key : out Key_Type);
      procedure Return_Vehicle (Tr : in Transportation);
      function Tc_Verify (What : Transportation) return Boolean;

   private
      Keys : Keys_Array;

   end Family_Transportation;

end Ca13001_1.Ca13001_2.Ca13001_4;

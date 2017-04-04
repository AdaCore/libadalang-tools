     --==================================================================--

package body Cc51b03_0 is

   package body Privateformalunknowndiscriminants is
      function Is_Definite return Boolean is
      begin
         if Formal'Definite then                -- Attribute used in "if"
            -- ...Execute algorithm #1...       -- condition inside subprogram.
            return True;
         else
            -- ...Execute algorithm #2...
            return False;
         end if;
      end Is_Definite;
   end Privateformalunknowndiscriminants;

   package body Taggedancestorunknowndiscriminants is
      function Is_Definite return Boolean is
      begin
         return Formal'Definite;                -- Attribute used in return
      end Is_Definite;                          -- statement inside subprogram.
   end Taggedancestorunknowndiscriminants;

end Cc51b03_0;

     --==================================================================--

with Cc51009_0;

package Cc51009_1 is

   generic
      type Element is private;

      type Set_Type is private;

      with package Sets is new Cc51009_0.Set_Signature (Element => Element,
         Set => Set_Type, others => <>);
   package Set_Processing is

      use Sets;

      procedure Test
        (Set_1 : in out Set_Type; Set_2 : Set_Type; Set_3 : Set_Type;
         Elt   :        Element);

   end Set_Processing;

end Cc51009_1;

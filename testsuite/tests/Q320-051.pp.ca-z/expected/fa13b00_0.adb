--==================================================================--

package body Fa13b00_0 is

   function Assign_Visible_Tagged
     (I : Visible_Integer) return Visible_Tagged
   is
      Vt : Visible_Tagged := (Pr => (Vi => I));
   begin
      return Vt;
   end Assign_Visible_Tagged;

   -------------------------------------------------------

   function Assign_Private_Tagged
     (I : Visible_Integer) return Private_Tagged
   is
      Pt : Private_Tagged := (Vi => I);
   begin
      return Pt;
   end Assign_Private_Tagged;

   -------------------------------------------------------

end Fa13b00_0;

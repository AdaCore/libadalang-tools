-- Alert_Foundation.Private_Child

--=======================================================================--

package F393b00.C393b14_1 is
   -- Alert_Foundation.Public_Child

   type Timing is (Before, After);
   procedure Init;
   procedure Modify;
   function Check_Before return Boolean;
   function Check_After return Boolean;

end F393b00.C393b14_1;

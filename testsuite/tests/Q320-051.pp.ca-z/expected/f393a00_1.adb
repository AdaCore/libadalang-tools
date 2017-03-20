with F393a00_0;
package body F393a00_1 is
   procedure Initialize (An_Object : in out Object) is
   begin
      An_Object.Initialized := True;
      F393a00_0.Tc_Touch ('a');
   end Initialize;

   function Initialized (An_Object : Object'Class) return Boolean is
   begin
      F393a00_0.Tc_Touch ('b');
      return An_Object.Initialized;
   end Initialized;
end F393a00_1;

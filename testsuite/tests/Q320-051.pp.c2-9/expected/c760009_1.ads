---------------------------------------------------------------- C760009_1

with Ada.Finalization;
package C760009_1 is

   Initialize_Called : Natural := 0;
   Adjust_Called     : Natural := 0;
   Finalize_Called   : Natural := 0;

   procedure Reset_Counters;

   type Simple_Control is new Ada.Finalization.Controlled with private;

   procedure Initialize (Av : in out Simple_Control);
   procedure Adjust (Av : in out Simple_Control);
   procedure Finalize (Av : in out Simple_Control);
   procedure Validate (Av : in out Simple_Control);

   function Item (Av : Simple_Control'Class) return String;

   Empty : constant Simple_Control;

   procedure Tc_Trace (Message : String);

private
   type Simple_Control is new Ada.Finalization.Controlled with record
      Item : Natural;
   end record;

   Empty : constant Simple_Control := (Ada.Finalization.Controlled with 0);

end C760009_1;

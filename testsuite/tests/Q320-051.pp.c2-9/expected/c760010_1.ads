---------------------------------------------------------------- C760010_1

with Ada.Finalization;
package C760010_1 is

   procedure Check_Counters (Init, Adj, Fin : Natural; Message : String);
   procedure Reset_Counters;

   type Simple_Control is new Ada.Finalization.Controlled with record
      Item : Integer;
   end record;
   procedure Initialize (Av : in out Simple_Control);
   procedure Adjust (Av : in out Simple_Control);
   procedure Finalize (Av : in out Simple_Control);

end C760010_1;

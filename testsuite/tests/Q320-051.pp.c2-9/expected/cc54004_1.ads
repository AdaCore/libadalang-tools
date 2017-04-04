     --===================================================================--

with Cc54004_0; use Cc54004_0;
package Cc54004_1 is

   type Low_Alert is new Cc54004_0.Alert with record
      C1 : String (1 .. 5) := "Dummy";
      -- ...Other components.
   end record;

   procedure Handle (A : in out Low_Alert);          -- Overrides parent's
   -- operations.
   --...Other operations.

   type Medium_Alert is new Low_Alert with record
      C : Integer := 6;
      -- ...Other components.
   end record;

   procedure Handle (A : in out Medium_Alert);       -- Overrides parent's
   -- operations.
   --...Other operations.

end Cc54004_1;

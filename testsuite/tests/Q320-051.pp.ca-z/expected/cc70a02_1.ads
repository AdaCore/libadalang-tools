-- No body for CC70A02_0.

     --==================================================================--

with Cc70a02_0;       -- Mathematical group signature.

package Cc70a02_1 is  -- Mathematical group operations.

   --                                  --
   -- Generic formal package used here --
   --                                  --

   generic            -- Powers for mathematical groups.
      with package Group is new Cc70a02_0 (<>);
   function Power
     (Left : Group.Group_Type; Right : Integer) return Group.Group_Type;

end Cc70a02_1;

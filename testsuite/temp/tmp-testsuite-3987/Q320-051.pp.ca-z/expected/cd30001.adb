------------------------------------------------------------------- CD30001

with Report;
with Cd30001_0;
procedure Cd30001 is

begin  -- Main test procedure.

   Report.Test
     ("CD30001",
      "Check that X'Address produces a useful result when X is " &
      "an aliased object, or an entity whose Address has been " &
      "specified");

--      Check that X'Address produces a useful result when X is an aliased
--      object.
--
--      Check that aliased objects and subcomponents are allocated on storage
--      element boundaries.  Check that objects and subcomponents of by
--      reference types are allocated on storage element boundaries.

   Cd30001_0.Tc_Check_Aliased_Addresses;

--      Check that X'Address produces a useful result when X is an entity
--      whose Address has been specified.

   Cd30001_0.Tc_Check_Specific_Addresses;

--      Check that X'Address produces a useful result when X is an object of
--      a by-reference type.

   Cd30001_0.Tc_Check_By_Reference_Types;

   Report.Result;

end Cd30001;

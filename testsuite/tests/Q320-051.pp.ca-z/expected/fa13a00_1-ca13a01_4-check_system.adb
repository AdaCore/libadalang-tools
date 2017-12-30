--==================================================================--

separate (Fa13a00_1.Ca13a01_4)

-- Subunit Check_System declared in Maintenance Operation.

procedure Check_System is
begin
   -- See if regular power is on.

   if Power /= V120 then                  -- Reference package with'ed by
      Tc_Operation := False;              -- the subunit parent's body.
   end if;

   -- Test elevator function.

   Fa13a00_1.Fa13a00_3.Move_Elevator      -- Reference public sibling of
     (Penthouse, Call_Waiting);           -- the subunit parent's body.

   if not Call_Waiting (Penthouse) then   -- Reference private part of the
      Tc_Operation := False;              -- parent of the subunit package's
      -- body.
   end if;

   Fa13a00_1.Fa13a00_2.Down (One_Floor);  -- Reference private sibling of
   -- the subunit parent's body.

   if Current_Floor /= Floor'Pred (Penthouse) then
      Tc_Operation := False;              -- Reference type declared in the
   end if;                                -- parent of the subunit parent's
   -- body.

end Check_System;

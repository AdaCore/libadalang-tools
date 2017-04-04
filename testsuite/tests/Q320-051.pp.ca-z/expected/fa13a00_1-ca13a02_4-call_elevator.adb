--==================================================================--

separate (Fa13a00_1.Ca13a02_4)

-- Subunit Call_Elevator declared in Outside Elevator Button Operations.

function Call_Elevator (D : Direction) return Light is
   Elevator_Button : Light;

begin
   -- See if power is on.

   if Power = Off then                       -- Reference package with'ed by
      Elevator_Button := Off;                -- the subunit parent's body.

   else
      case D is
         when Express =>
            Fa13a00_1.Fa13a00_3.Move_Elevator -- Reference public sibling of
            (Penthouse, Call_Waiting);      -- the subunit parent's body.

            Elevator_Button := Express;

         when Up =>
            if Current_Floor < Our_Floor then
               Fa13a00_1.Fa13a00_2.Up         -- Reference private sibling of
                 (Floor'Pos (Our_Floor)       -- the subunit parent's body.
                 - Floor'Pos (Current_Floor));
            else
               Fa13a00_1.Fa13a00_2.Down       -- Reference private sibling of
                 (Floor'Pos (Current_Floor)   -- the subunit parent's body.
                 - Floor'Pos (Our_Floor));
            end if;

            -- Call elevator.

            Call
              (Current_Floor,
               Call_Waiting);  -- Reference subprogram declared
            -- in the parent of the subunit parent's body.
            Elevator_Button := Up;

         when Down =>
            if Current_Floor > Our_Floor then
               Fa13a00_1.Fa13a00_2.Down       -- Reference private sibling of
                 (Floor'Pos (Current_Floor)   -- the subunit parent's body.
                 - Floor'Pos (Our_Floor));
            else
               Fa13a00_1.Fa13a00_2.Up         -- Reference private sibling of
                 (Floor'Pos (Our_Floor)       -- the subunit parent's body.
                 - Floor'Pos (Current_Floor));
            end if;

            Elevator_Button := Down;

            -- Call elevator.

            Call
              (Current_Floor,
               Call_Waiting);  -- Reference subprogram declared
            -- in the parent of the subunit parent's body.
      end case;

      if not Call_Waiting (Current_Floor)     -- Reference private part of the
      then                                    -- parent of the subunit parent's
         -- body.
         Tc_Operation := False;
      end if;

   end if;

   return Elevator_Button;

end Call_Elevator;

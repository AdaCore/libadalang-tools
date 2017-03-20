--==================================================================--

separate (Ca13001_1.Ca13001_2.Ca13001_4)
protected body Family_Transportation is

   procedure Get_Vehicle (Who : in Family; Key : out Key_Type) is
   begin
      case Who is
         when Father | Mother =>
            -- Drive new car to work

            -- Reference package with'ed by the subunit parent's body.
            if Destination (Who) = Work then

               -- Reference type declared in the private parent of the subunit
               -- parent's body.
               -- Reference type declared in the visible part of the
               -- subunit parent's body.
               if not Vehicles (New_Car).In_Use and
                 Fuel (New_Car)

                  -- Reference type declared in the public sibling of the
                  -- subunit parent's body.
                   and
                 not Ca13001_1.Ca13001_2.Ca13001_3.Flat_Tire (New_Car)
               then
                  Vehicles (New_Car).In_Use := True;

                  -- Reference type declared in the private part of the
                  -- protected subunit.
                  Keys (New_Car).Available := False;
                  Key                      := Transportation'Pos (New_Car);
               else
                  -- Reference type declared in the grandparent of the subunit
                  -- parent's body.
                  Walking := True;
               end if;

            -- Drive clunker to other destinations.
            else
               if not Vehicles (Clunker).In_Use and
                 Fuel (Clunker) and
                 not Ca13001_1.Ca13001_2.Ca13001_3.Flat_Tire (Clunker)
               then
                  Vehicles (Clunker).In_Use := True;
                  Keys (Clunker).Available  := False;
                  Key                       := Transportation'Pos (Clunker);
               else
                  Walking := True;
                  Key     := Transportation'Pos (Bicycle);
               end if;
            end if;

         -- Similar for Teen.
         when Teen =>
            if not Vehicles (Clunker).In_Use and
              Fuel (Clunker) and
              not Ca13001_1.Ca13001_2.Ca13001_3.Flat_Tire (Clunker)
            then
               Vehicles (Clunker).In_Use := True;
               Keys (Clunker).Available  := False;
               Key                       := Transportation'Pos (Clunker);
            else
               Walking := True;
               Key     := Transportation'Pos (Bicycle);
            end if;
      end case;

   end Get_Vehicle;

   ----------------------------------------------------------------

   -- Any family member can bring back the transportation with the key.

   procedure Return_Vehicle (Tr : in Transportation) is
   begin
      Vehicles (Tr).In_Use := False;
      Keys (Tr).Available  := True;
   end Return_Vehicle;

   ----------------------------------------------------------------

   function Tc_Verify (What : Transportation) return Boolean is
   begin
      return Keys (What).Available;
   end Tc_Verify;

end Family_Transportation;

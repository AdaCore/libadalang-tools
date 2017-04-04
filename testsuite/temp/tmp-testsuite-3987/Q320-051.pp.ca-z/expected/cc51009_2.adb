     --==================================================================--

package body Cc51009_2 is

   package body Set_Abstraction is

      procedure Add (Elem : Element; To_Set : in out Set) is
         Found : Boolean := False;
      begin
         for E in To_Set.Elements'First .. To_Set.Last_Element loop
            if To_Set.Elements (E) = Elem then
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            To_Set.Last_Element                   := To_Set.Last_Element + 1;
            To_Set.Elements (To_Set.Last_Element) := Elem;
         end if;
      end Add;

      function Union (Left, Right : Set) return Set is
         Result : Set := Left;
         Found  : Boolean;

      begin
         for Er in Right.Elements'First .. Right.Last_Element loop
            Found := False;

            for El in Left.Elements'First .. Left.Last_Element loop
               if Left.Elements (El) = Right.Elements (Er) then
                  Found := True;

                  exit;
               end if;
            end loop;

            if not Found then
               Result.Last_Element := Result.Last_Element + 1;
               Result.Elements (Result.Last_Element) := Right.Elements (Er);
            end if;
         end loop;

         return Result;
      end Union;

      function Intersection (Left, Right : Set) return Set is
         Result : Set;
      begin
         for Er in Right.Elements'First .. Right.Last_Element loop
            for El in Left.Elements'First .. Left.Last_Element loop
               if Left.Elements (El) = Right.Elements (Er) then
                  Result.Last_Element := Result.Last_Element + 1;
                  Result.Elements (Result.Last_Element) := Right.Elements (Er);

                  exit;
               end if;
            end loop;
         end loop;

         return Result;
      end Intersection;

      function "=" (Left, Right : Set) return Boolean is
      begin
         if Left.Last_Element /= Right.Last_Element then
            return False;
         end if;

         for Er in Right.Elements'First .. Right.Last_Element loop
            declare
               Found_Element : Boolean := False;
            begin
               for El in Left.Elements'First .. Left.Last_Element loop
                  if Left.Elements (El) = Right.Elements (Er) then
                     Found_Element := True;
                     exit;
                  end if;
               end loop;

               if not Found_Element then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end "=";

   end Set_Abstraction;

end Cc51009_2;

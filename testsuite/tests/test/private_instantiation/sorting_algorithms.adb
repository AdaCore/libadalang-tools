package body Sorting_Algorithms is

   procedure Swap (L, R : in out Element_Type);

   procedure Swap (L, R : in out Element_Type) is
      X : Element_Type;
   begin
      X := L;
      L := R;
      R := X;
   end Swap;

   procedure Selection_Sort (X : in out Array_To_Sort) is
      Min_Index : Natural;
   begin
      for I in X'First .. X'Last - 1 loop

         Min_Index := I;

         for J in I + 1 .. X'Last loop
--              if X (J) < X (I) then
            if X (J) < X (Min_Index) then
               Min_Index := J;
            end if;
         end loop;

         if I /= Min_Index then
            Swap (X (I), X (Min_Index));
         end if;

      end loop;
   end Selection_Sort;

   procedure Bubble_Sort (X : in out Array_To_Sort) is
      Swapped : Boolean;
   begin
      for I in X'First .. X'Last - 1 loop
         Swapped := False;
         for J in X'First .. X'Last - 1 loop
            if X (J + 1) < X (j) then
               Swap (X (J), X (J + 1));
               Swapped := True;
            end if;
         end loop;
         exit when not Swapped;
      end loop;
   end Bubble_Sort;

end Sorting_Algorithms;

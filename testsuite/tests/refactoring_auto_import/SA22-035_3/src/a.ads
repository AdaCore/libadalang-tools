with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package A is

   package Integer_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Integer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   package IHM renames Integer_Hashed_Maps;

   procedure Subprogram;

   package B is

      package Integer_Hashed_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Integer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

      package IHM renames Integer_Hashed_Maps;

      procedure Subprogram;

      package C is

         package Integer_Hashed_Maps is new
           Ada.Containers.Indefinite_Hashed_Maps
             (Key_Type        => String,
              Element_Type    => Integer,
              Hash            => Ada.Strings.Hash,
              Equivalent_Keys => "=");

         package IHM renames Integer_Hashed_Maps;

         procedure Subprogram;

         package D is

            package Integer_Hashed_Maps is new
              Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type        => String,
                 Element_Type    => Integer,
                 Hash            => Ada.Strings.Hash,
                 Equivalent_Keys => "=");

            package IHM renames Integer_Hashed_Maps;

            procedure Subprogram;

         private

            package Private_Integer_Hashed_Maps is new
              Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type        => String,
                 Element_Type    => Integer,
                 Hash            => Ada.Strings.Hash,
                 Equivalent_Keys => "=");

            package PIHM renames Private_Integer_Hashed_Maps;

            procedure Private_Subprogram_1;

            procedure Private_Subprogram_2;

         end D;

      private

         package Private_Integer_Hashed_Maps is new
           Ada.Containers.Indefinite_Hashed_Maps
             (Key_Type        => String,
              Element_Type    => Integer,
              Hash            => Ada.Strings.Hash,
              Equivalent_Keys => "=");

         package PIHM renames Private_Integer_Hashed_Maps;

         procedure Private_Subprogram_1;

         procedure Private_Subprogram_2;

      end C;

   private

      package Private_Integer_Hashed_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps
          (Key_Type        => String,
           Element_Type    => Integer,
           Hash            => Ada.Strings.Hash,
           Equivalent_Keys => "=");

      package PIHM renames Private_Integer_Hashed_Maps;

      procedure Private_Subprogram_1;

      procedure Private_Subprogram_2;

   end B;

private


   package Private_Integer_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Integer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   package PIHM renames Private_Integer_Hashed_Maps;

   procedure Private_Subprogram_1;

   procedure Private_Subprogram_2;

end A;

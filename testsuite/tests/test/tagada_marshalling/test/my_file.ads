package My_File is

   type T1 is range 1 - 2 ** 31 .. 2 ** 31 - 1;
   type T2 is new Integer range 0 .. 100;
   type T3 is mod 2 ** 16;

   type Constr_Array is array (Positive range 1 .. 10) of Integer range 0 .. Integer'Last;

   type Matrix is array (Natural range <>, Character range <>) of Boolean;

   type Fixed_1 is delta 0.0001 range -0.1 .. 0.1;
   type Fixed_2 is delta 1_000_000.0 digits 16 range -100_000_000.0 .. 100_000_000.0;

   function Id (X : Integer) return Integer is (X);

   type Shape_Kind is (Point, Line, Circle, Square, Rectangle, Ellipse);
   subtype Name_Size_Ty is Natural range 0 .. 30;
   type Shape (K : Shape_Kind := Line; Name_Size : Name_Size_Ty := 30) is record
      Name  : String (1 .. Name_Size);
      X, Y  : Integer range -100 .. Id (100);
      case K is
         when Line =>
            X_2, Y_2 : Integer range Id (-100) .. 100;
         when Circle | Ellipse =>
            Radius   : Positive;
            case K is
               when Ellipse =>
                  Radius_2 : Positive;
               when others =>
                  null;
            end case;
         when Square .. Rectangle =>
            Side     : Positive;
            case K is
               when Rectangle =>
                  Side_2 : Positive;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end record;

   type Shape_Array is array (T2'Base range <>) of Shape;

   type Small_Shape_Array (L : T2 := 0) is record
      Content : Shape_Array (1 .. L);
   end record;

   type R is record
      F1 : T1;
      F2 : T2;
      F3 : T3;
      F4 : Boolean;
      F5 : Float;
      F6 : Long_Float;
      F7 : Long_Float; -- change back to long_long_float?
      F8 : Constr_Array;
      F9 : Character;
      G1 : Fixed_1;
      G2 : Fixed_2;
      G3 : Matrix (0 .. 9, 'A' .. 'I');
      G4 : Shape (Ellipse, 10);
      G5 : Shape;
      G6 : Small_Shape_Array;
   end record;

   type R2 is record
      F1, F2 : Boolean := False;
   end record with Predicate => F1 or F2;

   procedure Test (X : in out R; Y : T2; A : String; M : Matrix; Z : R2; D : Shape; V : Shape_Array) with Import;

   generic
      type T is range <>;
   function Ident (X : T) return T;

   function Ident (X : T) return T is (X);

   type T_Gen is range 1 .. 10;

   function Actual_Ident is new Ident (T => T_Gen);

   type T_Null is null record;

   procedure Use_Null_Rec (X : T_Null) with Import;

end My_File;

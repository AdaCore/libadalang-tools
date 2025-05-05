package Baar is
   type T is (One, Two);

   type Bar (Kind : T) is private;

private

   type Bar (Kind : T) is record
      X, Y : Integer;
      case Kind is
         when others => null;
      end case;
   end record;

end Baar;

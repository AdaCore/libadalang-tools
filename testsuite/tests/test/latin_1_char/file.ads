with Other_Package;

package File is

   type Name is private;

   type Optional_Name (Exists : Boolean := False) is record
      case Exists is
         when False =>
            null;
         when True  =>
            Value : Name;
      end case;
   end record;

   function Do_Something
     (Thing : in Other_Package.Name) return Optional_Name;

   function Do_Something2
     (Thing : in Name) return Optional_Name;

private
   type Name is record
      Other : Other_Package.Name;
   end record;
end File;

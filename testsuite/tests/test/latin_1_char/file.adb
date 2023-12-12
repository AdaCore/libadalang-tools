pragma Ada_2012;
package body File is

   ------------------
   -- Do_Something --
   ------------------

   function Do_Something (Thing : in Other_Package.Name) return Optional_Name
   is
   begin
      return raise Program_Error with "Unimplemented function Do_Something";
   end Do_Something;

   -------------------
   -- Do_Something2 --
   -------------------

   function Do_Something2 (Thing : in Name) return Optional_Name is
   begin
      return raise Program_Error with "Unimplemented function Do_Something2";
   end Do_Something2;

end File;

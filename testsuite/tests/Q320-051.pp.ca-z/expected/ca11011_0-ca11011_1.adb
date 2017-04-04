--=================================================================--

with Ca11011_0.Ca11011_1.Ca11011_2;  -- with Child OS.File_Manager.Internals

package body Ca11011_0.Ca11011_1 is  -- Package body OS.File_Manager

   package Internal renames Ca11011_0.Ca11011_1.Ca11011_2;

   -- This subprogram utilizes a call to a subprogram contained in a private
   -- child to perform the actual processing.

   procedure Create_File (File_Key : in File_Descriptor_Type) is
   begin
      Internal.Create (Key => File_Key);  -- Other parameters are defaults,
      -- since they are of private types from the parent package.
      -- File_Descriptor_Type is private, but declared in visible part
      -- of parent spec.
   end Create_File;

end Ca11011_0.Ca11011_1;        -- Package body OS.File_Manager

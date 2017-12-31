--=================================================================--

-- This package contains menu selectable operations for manipulating files.
-- This abstraction builds on the capabilities available from ancestor
-- packages.

package Ca11003_0.Ca11003_1.Ca11003_2 is

   procedure News (Mode : in File_Mode; File : out Extended_File_Type);

   procedure Copy (Original : in     Extended_File_Type;
      Duplicate             :    out Extended_File_Type);

   procedure Delete (File : in Extended_File_Type);

end Ca11003_0.Ca11003_1.Ca11003_2;  -- Grandchild package OS.Operations.Menu

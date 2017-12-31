--=================================================================--

with Ca11010_0;                        -- with Package OS.
with Report;

procedure Ca11010 is

   package Os renames Ca11010_0;

   Ada_File_Key          : Os.File_Descriptor_Type := Os.Default_Descriptor;
   Initialization_Status : Boolean                 := False;

begin

   -- This test indicates one approach to a file management operation. It is
   -- not intended to demonstrate full functionality, but rather that the use
   -- of a private child package can provide a solution to a user situation,
   -- that being the implementation of certain functions being provided
   -- in a child package, with the parent package body utilizing these
   -- implementations.

   Report.Test
     ("CA11010",
      "Check that a private child package can use " &
      "entities declared in the private part of its " & "parent unit");

   -- Check initial conditions of the first entry in the file table. These are
   -- all default values provided in the declaration of the type File_Type.

   Os.Verify_Initial_Conditions (Initialization_Status);

   if not Initialization_Status then
      Report.Failed ("Initial condition failure");
   end if;

   -- Call the initialization function. This will result in the resetting of
   -- the fields associated with the first entry in the File_Table (this is the
   -- first/only call of Initialize_File). No parameters are necessary for this
   -- call, due to the default values provided in the private child package
   -- routine Initialize.

   Ada_File_Key := Os.Initialize_File;

   -- Verify that the initial conditions of the file table component have been
   -- properly modified by the initialization function.

   if not Os.Final_Conditions_Valid then
      Report.Failed ("Initialization processing failure");
   end if;

   Report.Result;

end Ca11010;

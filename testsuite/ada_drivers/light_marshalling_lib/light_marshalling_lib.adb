with Ada.Command_Line;

with Ada.Text_IO;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

with Test.Generation;
with Test.Common;

with TGen.Libgen;

procedure Light_Marshalling_Lib is
   package LAL renames Libadalang.Analysis;

   function Load_Project return LAL.Unit_Provider_Reference;

   function Load_Project return LAL.Unit_Provider_Reference is
      package GPR renames GNATCOLL.Projects;
      package LAL_GPR renames Libadalang.Project_Provider;
      use type GNATCOLL.VFS.Filesystem_String;

      Project_Filename : constant String := Ada.Command_Line.Argument (1);
      Project_File     : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Project_Filename);

      Env     : GPR.Project_Environment_Access;
      Project : constant GPR.Project_Tree_Access := new GPR.Project_Tree;
   begin
      GPR.Initialize (Env);
      Project.Load (Project_File, Env);
      return
        LAL_GPR.Create_Project_Unit_Provider (Tree => Project, Env => Env);
   end Load_Project;

   Output_Dir   : constant String := "obj/tgen_light";
   Context      : constant LAL.Analysis_Context :=
     LAL.Create_Context (Unit_Provider => Load_Project);
   TGen_Context : constant TGen.Libgen.Libgen_Context :=
     TGen.Libgen.Create (Output_Dir, ".", Ada.Command_Line.Argument (2));

begin
   --  Mock TGen context, to have a proper output dir
   Test.Common.TGen_Libgen_Ctx := TGen_Context;

   for I in 3 .. Ada.Command_Line.Argument_Count loop
      declare
         Filename : constant String := Ada.Command_Line.Argument (I);
         Unit     : constant LAL.Analysis_Unit :=
           Context.Get_From_File (Filename);
      begin
         Ada.Text_IO.Put_Line ("Process: " & Filename);
         Test.Generation.Process_Source (Unit);
      end;
   end loop;

   TGen.Libgen.Generate
     (Test.Common.TGen_Libgen_Ctx,
      [TGen.Libgen.Marshalling_Part => True, others => False]);
end Light_Marshalling_Lib;

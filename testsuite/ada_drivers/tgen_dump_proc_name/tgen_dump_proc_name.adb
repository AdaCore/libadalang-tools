with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Strings;

with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

with Test.Generation;
with Test.Common;

with TGen.Libgen;
with TGen.Strings;

procedure TGen_Dump_Proc_Name is
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

   Context : constant LAL.Analysis_Context :=
     LAL.Create_Context (Unit_Provider => Load_Project);

begin
   for I in 3 .. Ada.Command_Line.Argument_Count loop
      declare
         Filename : constant String := Ada.Command_Line.Argument (I);
         Unit     : constant LAL.Analysis_Unit :=
           Context.Get_From_File (Filename);
      begin
         Test.Generation.Process_Source (Unit);
      end;
   end loop;

   Ada.Text_IO.Put_Line
     (Ada.Strings.Unbounded.To_String
        (TGen.Libgen.Get_Test_Case_Dump_Procedure_Name
           (Test.Common.TGen_Libgen_Ctx,
            TGen.Strings.To_Qualified_Name ("User"),
            Ada.Strings.Unbounded.To_Unbounded_String ("User.Identity"))));
   Ada.Text_IO.Put_Line
     (Ada.Strings.Unbounded.To_String
        (TGen.Libgen.Get_Test_Case_Dump_Procedure_Name
           (Test.Common.TGen_Libgen_Ctx,
            TGen.Strings.To_Qualified_Name ("user_instantiation"),
            Ada.Strings.Unbounded.To_Unbounded_String
              ("TGen_Generic_Instantiation_user_instantiation.Instance."
               & "Plus_Two"))));
end TGen_Dump_Proc_Name;

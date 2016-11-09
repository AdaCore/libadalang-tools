with Text_IO;               use Text_IO;
with Ada.Wide_Text_IO; use Ada;
with Ada.Command_Line;      use Ada.Command_Line;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics;

with Libadalang;     use Libadalang;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;
with LAL_Extensions; use LAL_Extensions;

with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

procedure Contract_Coverage is

   --  Prototype contract-coverage metrics tool.

   --  Command line:
   --  contract_coverage [--verbose] source-files...

   --  Prints out the percentage of subprograms in a package visible part that
   --  have contracts (i.e. Pre, Post, or Contract_Cases aspects).

   --  Library subprograms are ignored. We process only subprograms in the
   --  visible part of a package or generic package. Package bodies and so on
   --  are silently ignored. Pragmas Precondition, Postcondition, and
   --  Contract_Cases are ignored -- only aspects count.

   --  See OB04-010 for requirements.

   Verbose : Boolean := False;
   --  Set by "--verbose" command-line switch; causes extra debugging output.

   Unit : Analysis_Unit;

   function Has_Contracts (Subp_Decl : Ada_Node) return Boolean;
   --  True if the subprogram has contract aspects

   subtype Percent is Float range 0.0 .. 100.0;

   function Get_Coverage (Pkg_Decl : Base_Package_Decl) return Percent;
   --  Returns the percentage of subprograms with contracts, so if all
   --  subprograms have contracts, 100.0 is returned. 100.0 is returned if
   --  there are no subprograms.

   function Has_Contracts (Subp_Decl : Ada_Node) return Boolean is
      Aspects : constant Aspect_Spec :=
        Get_Aspects (Basic_Decl (Subp_Decl));
   begin
      --  Search through the aspects, and return True if we find one of the
      --  relevant ones.

      if Aspects /= null then
         declare
            Assocs : constant Aspect_Assoc_List := F_Aspect_Assocs (Aspects);
         begin
            for I in 1 .. Child_Count (Assocs) loop
               declare
                  Assoc : constant Ada_Node := Childx (Assocs, I);
                  Nm : constant Expr := F_Id (Aspect_Assoc (Assoc));
               begin
                  if Kind (Nm) = Ada_Identifier then
                     declare
                        Text : constant W_Str := L_Name (Nm);
                     begin
                        if Text = "contract_cases"
                          or else Text = "pre"
                          or else Text = "post"
                        then
                           return True;
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;

      return False;
   end Has_Contracts;

   function Get_Coverage (Pkg_Decl : Base_Package_Decl) return Percent is
      Decls : constant Ada_Node_List := F_Decls (F_Public_Part (Pkg_Decl));

      Has_Contracts_Count : Natural := 0;
      --  Number of subprograms in the package with contracts

      Subp_Count : Natural := 0;
      --  Number of subprograms in the package.

      --  The result will be the ratio of the above two.

   begin
      if Decls /= null then -- Shouldn't it be empty list???
         for I in 1 .. Child_Count (Decls) loop
            declare
               Decl : constant Ada_Node := Childx (Decls, I);
            begin
               if Decl.all in Basic_Subprogram_Decl_Type'Class then
                  if Verbose then
                     Put ("    Doing subprogram ");
                     Wide_Text_IO.Put
                       (Full_Name
                         (F_Name
                           (F_Subp_Spec
                             (Basic_Subprogram_Decl (Decl)))));
                  end if;

                  Subp_Count := Subp_Count + 1;

                  if Has_Contracts (Decl) then
                     if Verbose then
                        Put_Line (": yes");
                     end if;

                     Has_Contracts_Count := Has_Contracts_Count + 1;

                  elsif Verbose then
                     Put_Line (": no");
                  end if;
               end if;
            end;
         end loop;
      end if;

      if Subp_Count = 0 then -- Don't divide by zero!
         return 100.0;
      else
         return (100.0 * Float (Has_Contracts_Count)) / Float (Subp_Count);
      end if;
   end Get_Coverage;

   Context : constant Analysis_Context :=
     Create (Charset => GNATCOLL.Iconv.UTF8);
   --  ???Charset is hardwired to UTF8 for now

   File_Count : Natural := 0;
   --  Used to complain if there are no file names on the command line

begin
   for Arg_Index in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (Arg_Index);
      begin
         if Arg = "--verbose" then
            Verbose := True;
            goto Continue;
         end if;
         if Verbose then
            Put_Line ("Doing file " & Arg);
         end if;

         File_Count := File_Count + 1;

         Unit := Get_From_File (Context, Filename => Arg);
         pragma Assert (Root (Unit) /= null);

         if Has_Diagnostics (Unit) then
            Put_Line ("Errors while parsing " & Arg);
            for D of Diagnostics (Unit) loop
               Put_Line (Langkit_Support.Diagnostics.To_Pretty_String (D));
            end loop;
         end if;

         --  We calculate metrics even in the presence of errors

         declare
            function Is_Pkg_Or_Gen
              (Node : access Ada_Node_Type'Class) return Boolean is
                (Kind (Node) in Ada_Base_Package_Decl | Ada_Package_Decl);
            --  True if the node is a generic package declaration or a
            --  package declaration.

            Packs : constant Ada_Node_Vectors.Elements_Array :=
              Find_All (Root (Unit), Is_Pkg_Or_Gen'Access);

         begin
            if Verbose then
               Print (Unit);
            end if;

            --  Go through all the [generic] package declarations in the
            --  file, calculate the coverage, and print it out.

            for P : Ada_Node of Packs loop
               declare
                  Pkg_Decl : constant Base_Package_Decl :=
                    Base_Package_Decl (P);
                  Coverage : constant Percent := Get_Coverage (Pkg_Decl);
               begin
                  Put ("Contract coverage for package ");
                  Wide_Text_IO.Put
                    (Full_Name (F_Package_Name (Pkg_Decl)));
                  Put (":");
                  Put (Integer (Coverage)'Img);
                  Put_Line ("%");
               end;
            end loop;
         end;
--      exception
--         when others =>
--            Put_Line ("Contract coverage analysis failed for file " & Arg);
      end;

      <<Continue>>
   end loop;

   if File_Count = 0 then
      Put_Line ("Missing command-line arguments");
   end if;
end Contract_Coverage;

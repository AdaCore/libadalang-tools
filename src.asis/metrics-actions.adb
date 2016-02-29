with Ada.Wide_Wide_Characters.Handling;
with Text_IO;               use Text_IO;
with Ada.Wide_Wide_Text_IO; use Ada;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang;     use Libadalang;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL.Common;

pragma Warnings (Off);
with LAL_UL.Projects;
with LAL_UL.Drivers;
pragma Warnings (On);

package body METRICS.Actions is

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   function Has_Contracts (Subp_Decl : Ada_Node) return Boolean;
   --  True if the subprogram has contract aspects

   subtype Percent is Float range 0.0 .. 100.0;

   function Get_Coverage
     (Cmd : Command_Line; Pkg_Decl : Base_Package_Decl) return Percent;
   --  Returns the percentage of subprograms with contracts, so if all
   --  subprograms have contracts, 100.0 is returned. 100.0 is returned if
   --  there are no subprograms.

   function Has_Contracts (Subp_Decl : Ada_Node) return Boolean is
      Aspects : constant Aspect_Specification :=
        F_Aspects (Subprogram_Decl (Subp_Decl));
   begin
      --  Search through the aspects, and return True if we find one of the
      --  relevant ones.

      if Aspects /= null then
         declare
            Assocs : constant List_Aspect_Assoc := F_Aspect_Assocs (Aspects);
         begin
            for I in 0 .. Child_Count (Assocs) - 1 loop
               declare
                  Assoc : constant Ada_Node := Childx (Assocs, I);
                  use Ada.Wide_Wide_Characters.Handling;
                  Id : constant Expr := F_Id (Aspect_Assoc (Assoc));
               begin
                  if Kind (Id) = Identifier_Kind then
                     declare
                        Text : constant Text_Type :=
                          To_Lower (F_Tok (Single_Tok_Node (Id)).Text.all);
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

   function Get_Coverage
     (Cmd : Command_Line; Pkg_Decl : Base_Package_Decl) return Percent is
      Decls : constant List_Ada_Node := F_Decls (Pkg_Decl);

      Has_Contracts_Count : Natural := 0;
      --  Number of subprograms in the package with contracts

      Subp_Count : Natural := 0;
      --  Number of subprograms in the package.

      --  The result will be the ratio of the above two.

   begin
      if Decls /= null then -- Shouldn't it be empty list???
         for I in 0 .. Child_Count (Decls) - 1 loop
            declare
               Decl : constant Ada_Node := Childx (Decls, I);
            begin
               if Kind (Decl) = Subprogram_Decl_Kind then
                  if Arg (Cmd, Verbose) then
                     Put ("    Doing subprogram ");
                     Wide_Wide_Text_IO.Put
                       (Full_Name
                          (F_Name (F_Subp_Spec (Subprogram_Decl (Decl)))));
                  end if;

                  Subp_Count := Subp_Count + 1;

                  if Has_Contracts (Decl) then
                     if Arg (Cmd, Verbose) then
                        Put_Line (": yes");
                     end if;

                     Has_Contracts_Count := Has_Contracts_Count + 1;

                  elsif Arg (Cmd, Verbose) then
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

   procedure Per_File_Action
     (Cmd : Command_Line; File_Name : String; Unit : Analysis_Unit) is

      pragma Unreferenced (File_Name);

      function Is_Pkg_Or_Gen (Node : Ada_Node) return Boolean is
        (Kind (Node) in Base_Package_Decl_Kind | Package_Decl_Kind);
      --  True if the node is a generic package declaration or a
      --  package declaration.

      Packs : constant Ada_Node_Vectors.Elements_Array :=
        Find_All (Root (Unit), Is_Pkg_Or_Gen'Access);

   begin
      if Arg (Cmd, Verbose) then
         Print (Unit);
      end if;

      --  Go through all the [generic] package declarations in the
      --  file, calculate the coverage, and print it out.

      for P : Ada_Node of Packs loop
         declare
            Pkg_Decl : constant Base_Package_Decl :=
              Base_Package_Decl (P);
            Coverage : constant Percent := Get_Coverage (Cmd, Pkg_Decl);
         begin
            Put ("Contract coverage for package ");
            Wide_Wide_Text_IO.Put
              (Full_Name (F_Package_Name (Pkg_Decl)));
            Put (":");
            Put (Integer (Coverage)'Img);
            Put_Line ("%");
         end;
      end loop;
   end Per_File_Action;

end METRICS.Actions;

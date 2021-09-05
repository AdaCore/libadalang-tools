------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2011-2021, AdaCore                    --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------
pragma Ada_2012;

with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Test.Skeleton.Source_Table is

   Me         : constant Trace_Handle :=
     Create ("Skeletons.Sources", Default => Off);
   Me_Verbose : constant Trace_Handle :=
     Create ("Skeletons.Sources_Verbose", Default => Off);

   -----------------------
   -- Source File table --
   -----------------------

   Sources_Left  : Natural := 0;
   Total_Sources : Natural := 0;

   type SF_Record;

   type SF_Record is record

      Full_Source_Name : String_Access;
      --  This field stores the source name with full directory information
      --  in absolute form

      Suffixless_Name : String_Access;
      --  The source name without directory information and suffix (if any)
      --  is used to create the names of the tree file and ALI files

      Test_Destination : String_Access;
      --  The path to the corresponding test unit location.

      Stub_Destination : String_Access;
      --  The path to the corresponding stub unit location.

      Status : SF_Status;
      --  Status of the given source. Initially is set to Waiting, then is
      --  changed according to the results of the metrics computation

      Corresponding_Body : String_Access := null;
      --  Set in Stub Mode for package specs.

      Stub_Data_Base_Spec : String_Access;
      Stub_Data_Base_Body : String_Access;
      --  Different projects in the hierarchy may have different naming
      --  schemes, but we won't have the access to this info once ASIS context
      --  is generated, so we need to calculate those names beforehand.

      Stub_Created : Boolean := False;

      Project_Name : String_Access;
      --  Name of corresponding project. Only relevant for bodies.
      Unit_Name : String_Access := null;
   end record;

   package Source_File_Table is new
     Ada.Containers.Indefinite_Ordered_Maps (String, SF_Record);

   Current_Source : String_Access := null;

   use String_Set;

   use Source_File_Table;

   package Source_File_Locations renames String_Set;

   SF_Table : Source_File_Table.Map;
   --  Source Table itself

   SFL_Table : Source_File_Locations.Set;
   --  A set of paths to source files. Used for creation of project file.

   SF_Process_Iterator  : Source_File_Table.Cursor;
   SF_Access_Iterator   : Source_File_Table.Cursor;
   SFL_Iterator         : Source_File_Locations.Cursor;

   Short_Source_Name_String : String_Access;
   Full_Source_Name_String  : String_Access;

   procedure Reset_Source_Process_Iterator;
   --  Sets SF_Iterator to the begining of SF_Table.

   type Project_Record is record
      Path                : String_Access;
      Obj_Dir             : String_Access;
      Stub_Dir            : String_Access;
      Importing_List      : List_Of_Strings.List;
      Imported_List       : List_Of_Strings.List;
      Limited_Withed      : String_Set.Set;
      Is_Externally_Built : Boolean;
      Is_Library          : Boolean := False;

      Needed_For_Extention : Boolean := False;
   end record;

   use List_Of_Strings;

   package Project_File_Table is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Project_Record);
   use Project_File_Table;

   PF_Table : Project_File_Table.Map;

   function Is_Body (Source_Name : String) return Boolean;

   -----------------------------
   --  Add_Source_To_Process  --
   -----------------------------

   procedure Add_Source_To_Process (Fname : String) is
      First_Idx : Natural;
      Last_Idx  : Natural;

      New_SF_Record : SF_Record;
   begin
      Trace (Me, "adding source: " & Fname);

      if not Is_Regular_File (Fname) then
         Report_Std ("gnattest: " & Fname & " not found");
         return;
      end if;

      --  Check if we already have a file with the same short name:
      Short_Source_Name_String := new String'(Base_Name (Fname));
      Full_Source_Name_String  :=
        new String'(Normalize_Pathname
          (Fname,
           Resolve_Links  => False,
             Case_Sensitive => False));

      if
        Source_Present (Full_Source_Name_String.all) and then
        Get_Source_Status (Full_Source_Name_String.all) = Body_Reference
      then
         Trace (Me, "...replacing body reference");
         New_SF_Record := SF_Table.Element (Full_Source_Name_String.all);
         SF_Table.Delete (Full_Source_Name_String.all);
         New_SF_Record.Status := Waiting;
         Insert (SF_Table, Full_Source_Name_String.all, New_SF_Record);
         return;
      elsif Source_Present (Full_Source_Name_String.all) then
         --  Duplicate, just ignore it
         return;
      end if;

      --  Making the new SF_Record
      New_SF_Record.Full_Source_Name :=
        new String'(Full_Source_Name_String.all);

      First_Idx := Short_Source_Name_String'First;
      Last_Idx  := Short_Source_Name_String'Last;

      for J in reverse  First_Idx + 1 .. Last_Idx loop

         if Short_Source_Name_String (J) = '.' then
            Last_Idx := J - 1;
            exit;
         end if;

      end loop;

      New_SF_Record.Suffixless_Name :=
        new String'(Short_Source_Name_String.all (First_Idx .. Last_Idx));

      New_SF_Record.Status := Waiting;

      if Stub_Mode_ON then
         declare
            Given_File : constant GNATCOLL.VFS.Virtual_File :=
              Create (+Fname);
            Other_File : constant GNATCOLL.VFS.Virtual_File :=
              Source_Project_Tree.Other_File (Given_File);
            F_Info     : constant File_Info                 :=
              Source_Project_Tree.Info (Given_File);
            P : Project_Type;
         begin
            if Given_File /= Other_File
              and then Is_Regular_File (Other_File.Display_Full_Name)
            then
               New_SF_Record.Corresponding_Body :=
                 new String'(Other_File.Display_Full_Name);
            end if;

            New_SF_Record.Stub_Data_Base_Spec := new String'
              (+
                 (File_From_Unit
                      (Project         => F_Info.Project,
                       Unit_Name       =>
                         F_Info.Unit_Name & "." & Stub_Data_Unit_Name,
                       Part            => Unit_Spec,
                       Language        => F_Info.Language,
                       File_Must_Exist => False)));

            New_SF_Record.Stub_Data_Base_Body := new String'
              (+
                 (File_From_Unit
                      (Project         => F_Info.Project,
                       Unit_Name       =>
                         F_Info.Unit_Name & "." & Stub_Data_Unit_Name,
                       Part            => Unit_Body,
                       Language        => F_Info.Language,
                       File_Must_Exist => False)));

            P := F_Info.Project;
            loop
               exit when Extending_Project (P) = No_Project;
               P := Extending_Project (P);
            end loop;

            New_SF_Record.Project_Name := new String'(P.Name);
         end;

      end if;

      Insert (SF_Table, Full_Source_Name_String.all, New_SF_Record);

      Include
        (SFL_Table,
         Normalize_Pathname (Name => Dir_Name (Full_Source_Name_String.all),
                             Resolve_Links  => False,
                             Case_Sensitive => False));

      Sources_Left  := Sources_Left + 1;
      Total_Sources := Total_Sources + 1;

      Free (Short_Source_Name_String);
      Free (Full_Source_Name_String);

   end Add_Source_To_Process;

   -------------------------
   -- Add_Body_To_Process --
   -------------------------

   procedure Add_Body_To_Process
     (Fname : String; Pname : String; Uname : String)
   is
      First_Idx : Natural;
      Last_Idx  : Natural;

      New_SF_Record : SF_Record;
   begin
      Trace (Me, "adding " & Fname & " from project " & Pname);
      --  Check if we already have a file with the same short name:
      Short_Source_Name_String := new String'(Base_Name (Fname));
      Full_Source_Name_String  :=
        new String'(Normalize_Pathname
                    (Fname,
                       Resolve_Links  => False,
                       Case_Sensitive => False));

      --  Making the new SF_Record
      New_SF_Record.Full_Source_Name :=
        new String'(Full_Source_Name_String.all);

      First_Idx := Short_Source_Name_String'First;
      Last_Idx  := Short_Source_Name_String'Last;

      for J in reverse  First_Idx + 1 .. Last_Idx loop

         if Short_Source_Name_String (J) = '.' then
            Last_Idx := J - 1;
            exit;
         end if;

      end loop;

      New_SF_Record.Suffixless_Name :=
        new String'(Short_Source_Name_String.all (First_Idx .. Last_Idx));

      New_SF_Record.Status := To_Stub_Body;

      New_SF_Record.Project_Name := new String'(Pname);
      New_SF_Record.Unit_Name := new String'(Uname);

      Insert (SF_Table, Full_Source_Name_String.all, New_SF_Record);

      Include
        (SFL_Table,
         Normalize_Pathname (Name => Dir_Name (Full_Source_Name_String.all),
                             Resolve_Links  => False,
                             Case_Sensitive => False));

      Free (Short_Source_Name_String);
      Free (Full_Source_Name_String);
   end Add_Body_To_Process;

   ------------------------
   -- Add_Body_Reference --
   ------------------------

   procedure Add_Body_Reference (Fname : String) is
      First_Idx : Natural;
      Last_Idx  : Natural;

      New_SF_Record : SF_Record;
   begin
      Trace (Me, "adding source (as reference): " & Fname);

      if not Is_Regular_File (Fname) then
         Report_Std ("gnattest: " & Fname & " not found");
         return;
      end if;

      Short_Source_Name_String := new String'(Base_Name (Fname));
      Full_Source_Name_String  :=
        new String'(Normalize_Pathname
          (Fname,
           Resolve_Links  => False,
             Case_Sensitive => False));

      --  Already present specs should not be overridden
      if
        SF_Table.Find
          (Full_Source_Name_String.all) /= Source_File_Table.No_Element
      then
         return;
      end if;

      --  Making the new SF_Record
      New_SF_Record.Full_Source_Name :=
        new String'(Full_Source_Name_String.all);

      First_Idx := Short_Source_Name_String'First;
      Last_Idx  := Short_Source_Name_String'Last;

      for J in reverse  First_Idx + 1 .. Last_Idx loop

         if Short_Source_Name_String (J) = '.' then
            Last_Idx := J - 1;
            exit;
         end if;

      end loop;

      New_SF_Record.Suffixless_Name :=
        new String'(Short_Source_Name_String.all (First_Idx .. Last_Idx));

      New_SF_Record.Status := Body_Reference;

      declare
         Given_File : constant GNATCOLL.VFS.Virtual_File :=
           Create (+Fname);
         Other_File : constant GNATCOLL.VFS.Virtual_File :=
           Source_Project_Tree.Other_File (Given_File);
         F_Info     : constant File_Info                 :=
           Source_Project_Tree.Info (Given_File);
         P : Project_Type;
      begin
         if Given_File /= Other_File
           and then Is_Regular_File (Other_File.Display_Full_Name)
         then
            New_SF_Record.Corresponding_Body :=
              new String'(Other_File.Display_Full_Name);
         end if;

         New_SF_Record.Stub_Data_Base_Spec := new String'
           (+
              (File_From_Unit
                   (Project         => F_Info.Project,
                    Unit_Name       =>
                      F_Info.Unit_Name & "." & Stub_Data_Unit_Name,
                    Part            => Unit_Spec,
                    Language        => F_Info.Language,
                    File_Must_Exist => False)));

         New_SF_Record.Stub_Data_Base_Body := new String'
           (+
              (File_From_Unit
                   (Project         => F_Info.Project,
                    Unit_Name       =>
                      F_Info.Unit_Name & "." & Stub_Data_Unit_Name,
                    Part            => Unit_Body,
                    Language        => F_Info.Language,
                    File_Must_Exist => False)));

         P := F_Info.Project;
         loop
            exit when Extending_Project (P) = No_Project;
            P := Extending_Project (P);
         end loop;
         New_SF_Record.Project_Name := new String'(P.Name);
         New_SF_Record.Unit_Name := new String'(F_Info.Unit_Name);
      end;

      Insert (SF_Table, Full_Source_Name_String.all, New_SF_Record);

      Free (Short_Source_Name_String);
      Free (Full_Source_Name_String);
   end Add_Body_Reference;

   ----------------------
   --  SF_Table_Empty  --
   ----------------------

   function SF_Table_Empty return Boolean is
      Empty : constant Boolean := Is_Empty (SF_Table);
      Cur   : Source_File_Table.Cursor;
   begin
      if Empty then
         return Empty;
      else
         Cur := SF_Table.First;
         while Cur /= Source_File_Table.No_Element loop
            if Element (Cur).Status /= To_Stub_Body then
               return False;
            end if;

            Next (Cur);
         end loop;

         return True;
      end if;
   end SF_Table_Empty;

   ---------------------------
   -- Get_Imported_Projects --
   ---------------------------

   function Get_Imported_Projects (Project_Name : String)
                                   return List_Of_Strings.List
   is
   begin
      return Project_File_Table.Element
        (PF_Table, Project_Name).Imported_List;
   end Get_Imported_Projects;

   ----------------------------
   -- Get_Importing_Projects --
   ----------------------------

   function Get_Importing_Projects (Project_Name : String)
                                    return List_Of_Strings.List
   is
   begin
      return Project_File_Table.Element
        (PF_Table, Project_Name).Importing_List;
   end Get_Importing_Projects;

   ----------------------
   -- Get_Project_Path --
   ----------------------

   function Get_Project_Path (Project_Name : String) return String is
   begin
      return Project_File_Table.Element
        (PF_Table, Project_Name).Path.all;
   end Get_Project_Path;

   --------------------------
   -- Get_Project_Stub_Dir --
   --------------------------

   function Get_Project_Stub_Dir (Project_Name : String) return String is
   begin
      return Project_File_Table.Element
        (PF_Table, Project_Name).Stub_Dir.all;
   end Get_Project_Stub_Dir;

   ---------------------
   -- Get_Source_Body --
   ---------------------

   function Get_Source_Body (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
      SFR : SF_Record;
   begin
      if Source_Present (SN) then
         SFR := Source_File_Table.Element (SF_Table, SN);
      else
         Report_Std
           ("warning: (gnattest) "
            & Source_Name
            & " is not a source of argument project, cannot create stub");

         return "";
      end if;

      if SFR.Corresponding_Body = null then
         return "";
      else
         return SFR.Corresponding_Body.all;
      end if;
   end Get_Source_Body;

   -----------------------------
   --  Get_Source_Output_Dir  --
   -----------------------------
   function Get_Source_Output_Dir (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
      SR : constant SF_Record := Source_File_Table.Element (SF_Table, SN);
   begin
      if SR.Test_Destination = null then
         return "";
      else
         return SR.Test_Destination.all;
      end if;
   end Get_Source_Output_Dir;

   ------------------------
   -- Get_Source_Project --
   ------------------------

   function Get_Source_Project_Name (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Project_Name.all;
   end Get_Source_Project_Name;

   --------------------------
   -- Get_Source_Unit_Name --
   --------------------------

   function Get_Source_Unit_Name (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Unit_Name.all;
   end Get_Source_Unit_Name;

   -------------------------
   -- Get_Source_Stub_Dir --
   -------------------------

   function Get_Source_Stub_Dir (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Stub_Destination.all;
   end Get_Source_Stub_Dir;

   -------------------------------
   -- Get_Source_Stub_Data_Body --
   -------------------------------

   function Get_Source_Stub_Data_Body  (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Stub_Data_Base_Body.all;
   end Get_Source_Stub_Data_Body;

   -------------------------------
   -- Get_Source_Stub_Data_Spec --
   -------------------------------

   function Get_Source_Stub_Data_Spec  (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Stub_Data_Base_Spec.all;
   end Get_Source_Stub_Data_Spec;

   -------------------------
   --  Get_Source_Status  --
   -------------------------
   function Get_Source_Status (Source_Name : String) return SF_Status
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Status;
   end Get_Source_Status;

   ----------------------------------
   --  Get_Source_Suffixless_Name  --
   ----------------------------------
   function Get_Source_Suffixless_Name (Source_Name : String) return String
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Suffixless_Name.all;
   end Get_Source_Suffixless_Name;

   ------------------------------
   -- Initialize_Project_Table --
   ------------------------------

   procedure Initialize_Project_Table (Source_Project_Tree : Project_Tree) is
      Iter, Importing, Imported : Project_Iterator;
      P, P2 : Project_Type;

      Attr   : constant Attribute_Pkg_String := Build ("", "externally_built");
   begin
      Trace (Me, "Initialize_Project_Table");
      Increase_Indent (Me);
      Iter := Start (Source_Project_Tree.Root_Project);
      while Current (Iter) /= No_Project loop
         P := Current (Iter);
         Trace (Me, "processing " & P.Name);

         if Extending_Project (P) /= No_Project then
            --  We do not want extended projects in the table.
            goto Next_Project;
         end if;

         declare
            PR : Project_Record;
         begin
            if Has_Attribute (P, Attr) then
               if To_Lower (Attribute_Value (P, Attr)) = "true" then
                  PR.Is_Externally_Built := True;
                  --  Nothing should be done for sources of externally built
                  --  projects, so no point in calculating obj dirs and so on.
                  goto Add_Project;
               end if;
            end if;
            PR.Is_Externally_Built := False;

            if P = Source_Project_Tree.Root_Project then
               PR.Needed_For_Extention := True;
            end if;

            if Has_Attribute (P, Library_Name_Attribute)
              and then Attribute_Value (P, Library_Name_Attribute) /= ""
            then
               PR.Is_Library := True;
            end if;

            PR.Path := new String'(P.Project_Path.Display_Full_Name);
            PR.Obj_Dir := new String'(P.Object_Dir.Display_Full_Name);
            if Is_Absolute_Path (Stub_Dir_Name.all) then
               PR.Stub_Dir := new String'
                 (Stub_Dir_Name.all
                  & Directory_Separator
                  & P.Name);
            else
               PR.Stub_Dir := new String'
                 (P.Object_Dir.Display_Full_Name
                  & Stub_Dir_Name.all
                  & Directory_Separator
                  & P.Name);
            end if;

            Increase_Indent (Me, "imported projects:");
            P2 := P;

            while P2 /= No_Project loop
               Imported :=
                 P2.Start (Direct_Only => True, Include_Extended => False);

               while Current (Imported) /= No_Project loop
                  PR.Imported_List.Append (Current (Imported).Name);
                  if Is_Limited_With (Imported) then
                     PR.Limited_Withed.Include (Current (Imported).Name);
                  end if;
                     Trace (Me, Current (Imported).Name);
                  Next (Imported);
               end loop;

               P2 := Extended_Project (P2);
            end loop;
            Decrease_Indent (Me);

            Importing := P.Find_All_Projects_Importing (Direct_Only => True);
            Increase_Indent (Me, "importing projects:");
            while Current (Importing) /= No_Project loop
               PR.Importing_List.Append (Current (Importing).Name);
               Trace (Me, Current (Importing).Name);
               Next (Importing);
            end loop;
            Decrease_Indent (Me);

            <<Add_Project>>

            PF_Table.Include (P.Name, PR);
         end;

         <<Next_Project>>

         Next (Iter);
      end loop;
      Decrease_Indent (Me);
   end Initialize_Project_Table;

   -------------
   -- Is_Body --
   -------------

   function Is_Body (Source_Name : String) return Boolean
   is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Corresponding_Body = null;
   end Is_Body;

   ----------------------------------------
   -- Mark_Projects_With_Stubbed_Sources --
   ----------------------------------------

   procedure Mark_Projects_With_Stubbed_Sources is
      S_Cur : Source_File_Table.Cursor := SF_Table.First;
      PR : Project_Record;

      Processed_Projects : String_Set.Set;

      P_Cur : Project_File_Table.Cursor;

      procedure Process_Project (S : String);

      procedure Process_Project (S : String) is
         Cur : List_Of_Strings.Cursor;
         Local_PR : Project_Record;
      begin
         Trace (Me, "Process_Project " & S);
         if Processed_Projects.Contains (S) then
            return;
         end if;

         Processed_Projects.Include (S);

         if PF_Table.Element (S).Is_Externally_Built then
            --  Nothing to do for those.
            return;
         end if;

         Local_PR := PF_Table.Element (S);
         Local_PR.Needed_For_Extention := True;
         PF_Table.Replace (S, Local_PR);

         Cur := Local_PR.Importing_List.First;
         while Cur /= List_Of_Strings.No_Element loop
            Process_Project (List_Of_Strings.Element (Cur));
            Next (Cur);
         end loop;

      end Process_Project;
   begin
      Trace (Me, "Mark_Projects_With_Stubbed_Sources");
      Increase_Indent (Me);

      --  First, mark all projects that have sources that have been stubbed.
      while S_Cur /= Source_File_Table.No_Element loop
         if Source_File_Table.Element (S_Cur).Stub_Created then
            PR :=
              PF_Table.Element
                (Source_File_Table.Element (S_Cur).Project_Name.all);
            PR.Needed_For_Extention := True;

            Trace
              (Me,
               Source_File_Table.Element (S_Cur).Project_Name.all
               & " has stubbed sources");

            PF_Table.Replace
              (Source_File_Table.Element (S_Cur).Project_Name.all,
               PR);
         end if;

         Next (S_Cur);
      end loop;

      --  Now we need to also mark all projects that are imported by any
      --  of already marked ones.

      P_Cur := PF_Table.First;
      while P_Cur /= Project_File_Table.No_Element loop
         if
           not Processed_Projects.Contains (Project_File_Table.Key (P_Cur))
           and then Project_File_Table.Element (P_Cur).Needed_For_Extention
         then
            Process_Project (Project_File_Table.Key (P_Cur));
         end if;

         Next (P_Cur);
      end loop;

      Decrease_Indent (Me);
   end Mark_Projects_With_Stubbed_Sources;

   -------------------------
   -- Mark_Sourse_Stubbed --
   -------------------------

   procedure Mark_Sourse_Stubbed (Source_Name : String) is
      SF_Rec : SF_Record;
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      SF_Rec := Source_File_Table.Element (SF_Table, SN);
      SF_Rec.Stub_Created := True;
      Replace (SF_Table, SN, SF_Rec);
   end Mark_Sourse_Stubbed;

   ---------------------------------
   --  Next_Non_Processed_Source  --
   ---------------------------------
   function Next_Non_Processed_Source return String is
      Cur : Source_File_Table.Cursor := Source_File_Table.No_Element;
   begin
      Reset_Source_Process_Iterator;

      loop
         if Cur = Source_File_Table.No_Element and then
           Source_File_Table.Element (SF_Process_Iterator).Status = Pending
         then
            Cur := SF_Process_Iterator;
         end if;
         if
           Source_File_Table.Element (SF_Process_Iterator).Status = Waiting
         then
            Free (Current_Source);
            Current_Source := new String'(Key (SF_Process_Iterator));
            return Key (SF_Process_Iterator);
         end if;

         Next (SF_Process_Iterator);
         exit when SF_Process_Iterator = Source_File_Table.No_Element;
      end loop;

      if Cur /= Source_File_Table.No_Element then
         Free (Current_Source);
         Current_Source := new String'(Key (Cur));
         return Key (Cur);
      end if;

      Free (Current_Source);
      return "";
   end Next_Non_Processed_Source;

   -----------------------------
   -- Get_Current_Source_Spec --
   -----------------------------

   function Get_Current_Source_Spec return String is
   begin
      if Current_Source = null then
         return "";
      else
         return Current_Source.all;
      end if;
   end Get_Current_Source_Spec;

   ----------------------------
   --  Next_Source_Location  --
   ----------------------------
   function Next_Source_Location return String is
      Cur : Source_File_Locations.Cursor;
   begin
      if SFL_Iterator /= Source_File_Locations.No_Element then
         Cur := SFL_Iterator;
         Source_File_Locations.Next (SFL_Iterator);
         return Source_File_Locations.Element (Cur);
      else
         return "";
      end if;
   end Next_Source_Location;

   ------------------------
   --  Next_Source_Name  --
   ------------------------
   function Next_Source_Name return String is
      Cur : Source_File_Table.Cursor;
   begin
      if SF_Access_Iterator /= Source_File_Table.No_Element then
         Cur := SF_Access_Iterator;
         Source_File_Table.Next (SF_Access_Iterator);
         return Key (Cur);
      else
         return "";
      end if;
   end Next_Source_Name;

   ----------------------
   -- Project_Extended --
   ----------------------

   function Project_Extended (Project_Name : String) return Boolean is
   begin
      return Project_File_Table.Element
        (PF_Table, Project_Name).Needed_For_Extention;
   end Project_Extended;

   ------------------------
   -- Project_Is_Library --
   ------------------------

   function Project_Is_Library (Project_Name : String) return Boolean is
   begin
      return Project_File_Table.Element
        (PF_Table, Project_Name).Is_Library;
   end Project_Is_Library;

   -------------------------------
   --  Reset_Location_Iterator  --
   -------------------------------
   procedure Reset_Location_Iterator is
   begin
      SFL_Iterator := First (SFL_Table);
   end Reset_Location_Iterator;

   -----------------------------
   --  Reset_Source_Iterator  --
   -----------------------------
   procedure Reset_Source_Iterator is
   begin
      SF_Access_Iterator := First (SF_Table);
   end Reset_Source_Iterator;

   -------------------------------------
   --  Reset_Source_Process_Iterator  --
   -------------------------------------
   procedure Reset_Source_Process_Iterator is
   begin
      SF_Process_Iterator := First (SF_Table);
   end Reset_Source_Process_Iterator;

   ------------------
   --  Set_Status  --
   ------------------

   procedure Set_Source_Status (Source_Name : String;
                                New_Status : SF_Status) is
      SF_Rec : SF_Record;
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      SF_Rec := Source_File_Table.Element (SF_Table, SN);
      SF_Rec.Status := New_Status;
      Replace (SF_Table, SN, SF_Rec);
   end Set_Source_Status;

   -------------------------
   --  Set_Subdir_Output  --
   -------------------------

   procedure Set_Subdir_Output is
      SF_Rec     : SF_Record;
      Tmp_Str    : String_Access;
      SF_Rec_Key : String_Access;
      Cur        : Source_File_Table.Cursor := SF_Table.First;
   begin
      Increase_Indent (Me, "Set_Subdir_Output");

      loop
         exit when Cur = Source_File_Table.No_Element;

         SF_Rec := Source_File_Table.Element (Cur);
         SF_Rec_Key := new String'(Key (Cur));

         Trace (Me, "processing: " & SF_Rec_Key.all);

         Tmp_Str := new String'(Dir_Name (SF_Rec.Full_Source_Name.all));

         SF_Rec.Test_Destination :=
           new String'(Tmp_Str.all          &
                       Test_Subdir_Name.all &
                       Directory_Separator);

         Replace (SF_Table, SF_Rec_Key.all, SF_Rec);

         Source_File_Table.Next (Cur);
         Free (SF_Rec_Key);
         Free (Tmp_Str);
      end loop;

      Decrease_Indent (Me);

   end Set_Subdir_Output;

   -------------------------
   --  Set_Separate_Root  --
   -------------------------

   procedure Set_Separate_Root (Max_Common_Root : String) is
      SF_Rec     : SF_Record;
      Tmp_Str    : String_Access;
      SF_Rec_Key : String_Access;
      Cur        : Source_File_Table.Cursor := SF_Table.First;

      Idx : Integer;
   begin
      Increase_Indent (Me, "Set_Separate_Root");

      loop
         exit when  Cur = Source_File_Table.No_Element;

         SF_Rec := Source_File_Table.Element (Cur);
         SF_Rec_Key := new String'(Key (Cur));

         Trace (Me, "processing: " & SF_Rec_Key.all);

         Tmp_Str := new String'(Dir_Name (SF_Rec.Full_Source_Name.all));

         Idx := Max_Common_Root'Last + 1;

         SF_Rec.Test_Destination :=
           new String'(Separate_Root_Dir.all &
                       Directory_Separator   &
                       Tmp_Str.all (Idx .. Tmp_Str.all'Last));

         Replace (SF_Table, SF_Rec_Key.all, SF_Rec);

         Source_File_Table.Next (Cur);
         Free (SF_Rec_Key);
         Free (Tmp_Str);
      end loop;

      Decrease_Indent (Me);

   end Set_Separate_Root;

   -----------------------
   -- Set_Direct_Output --
   -----------------------

   procedure Set_Direct_Output is
      SF_Rec     : SF_Record;
      Tmp_Str    : String_Access;
      SF_Rec_Key : String_Access;
      Cur        : Source_File_Table.Cursor := SF_Table.First;

      Project : Project_Type;

      TD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Test_Dir_Name.all);
   begin

      loop
         exit when  Cur = Source_File_Table.No_Element;

         SF_Rec := Source_File_Table.Element (Cur);
         SF_Rec_Key := new String'(Key (Cur));

         if TD_Name.Is_Absolute_Path then
            SF_Rec.Test_Destination := new String'(Test_Dir_Name.all);
         else
            Project := GNATCOLL.Projects.Project (Info
              (Source_Project_Tree,
               GNATCOLL.VFS.Create (+SF_Rec.Full_Source_Name.all)));
            SF_Rec.Test_Destination := new String'
              (Project.Object_Dir.Display_Full_Name & Test_Dir_Name.all);
         end if;

         Replace (SF_Table, SF_Rec_Key.all, SF_Rec);

         Source_File_Table.Next (Cur);
         Free (SF_Rec_Key);
         Free (Tmp_Str);
      end loop;
   end Set_Direct_Output;

   ----------------------------
   -- Set_Direct_Stub_Output --
   ----------------------------

   procedure Set_Direct_Stub_Output is
      SF_Rec     : SF_Record;
      Tmp_Str    : String_Access;
      SF_Rec_Key : String_Access;
      Cur        : Source_File_Table.Cursor := SF_Table.First;

      Project : Project_Type;

      TD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Stub_Dir_Name.all);
   begin

      loop
         exit when  Cur = Source_File_Table.No_Element;

         SF_Rec := Source_File_Table.Element (Cur);
         SF_Rec_Key := new String'(Key (Cur));

         Project := GNATCOLL.Projects.Project
           (Info (Source_Project_Tree,
            GNATCOLL.VFS.Create (+SF_Rec.Full_Source_Name.all)));

         loop
            exit when Extending_Project (Project) = No_Project;
            Project := Extending_Project (Project);
         end loop;

         --  Better use subdirs to separate stubs from different projects.
         if TD_Name.Is_Absolute_Path then
            SF_Rec.Stub_Destination := new String'
              (Stub_Dir_Name.all
               & Directory_Separator
               & Project.Name);
         else
            SF_Rec.Stub_Destination := new String'
              (Project.Object_Dir.Display_Full_Name
               & Stub_Dir_Name.all
               & Directory_Separator
               & Project.Name);
         end if;

         Replace (SF_Table, SF_Rec_Key.all, SF_Rec);

         Source_File_Table.Next (Cur);
         Free (SF_Rec_Key);
         Free (Tmp_Str);
      end loop;
   end Set_Direct_Stub_Output;

   --------------------
   -- Set_Output_Dir --
   --------------------

   procedure Set_Output_Dir (Source_Name : String; Output_Dir : String) is
      SF_Rec : SF_Record;
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      Trace (Me, "Set_Output_Dir for " & Source_Name);
      SF_Rec := SF_Table.Element (SN);
      SF_Rec.Test_Destination := new String'(Output_Dir);
      Replace (SF_Table, SN, SF_Rec);
   end Set_Output_Dir;

   ----------------------
   --  Source_Present  --
   ----------------------
   function Source_Present (Source_Name : String) return Boolean is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Contains (SF_Table, SN);
   end Source_Present;

   --------------------
   -- Source_Stubbed --
   --------------------

   function Source_Stubbed (Source_Name : String) return Boolean is
      SN : constant String :=
        Normalize_Pathname
          (Name           => Source_Name,
           Resolve_Links  => False,
           Case_Sensitive => False);
   begin
      return Source_File_Table.Element
        (SF_Table, SN).Stub_Created;
   end Source_Stubbed;

   --------------------------------------
   -- Enforce_Custom_Project_Extention --
   --------------------------------------

   procedure Enforce_Custom_Project_Extention
     (File_Name            : String;
      Subroot_Stub_Prj     : String;
      Current_Source_Infix : String)
   is
      Short_Name : constant String := Base_Name (File_Name);

      Excluded_Sources             : String_Set.Set := String_Set.Empty_Set;
      Current_Proj_Present_Sources : String_Set.Set := String_Set.Empty_Set;
      Processed_Projects           : String_Set.Set := String_Set.Empty_Set;

      SS_Cur  : String_Set.Cursor;
      Subroot_Prj_Name : constant String :=
        Get_Source_Project_Name (File_Name);

      procedure Process_Project (Proj : String);

      procedure Set_Present_Subset_For_Project (Proj : String);

      procedure Process_Project (Proj : String) is
         Cur, I_Cur : List_Of_Strings.Cursor;
         E_Cur : String_Set.Cursor;
         Arg_Proj : Project_Record;

         Relative_P_Path, Relative_I_Path : String_Access;
      begin
         if Processed_Projects.Contains (Proj) then
            return;
         end if;
         Processed_Projects.Include (Proj);
         Arg_Proj := PF_Table.Element (Proj);

         if Proj = Subroot_Prj_Name then
            --  The root of the subtree is extended by the test driver project.
            goto Process_Imported;
         end if;

         if Arg_Proj.Needed_For_Extention then

            declare
               F : File_Array_Access;
            begin
               Append
                 (F,
                  GNATCOLL.VFS.Create
                    (+(Arg_Proj.Stub_Dir.all)));
               Append
                 (F,
                  GNATCOLL.VFS.Create
                    (+(Arg_Proj.Stub_Dir.all
                     & Directory_Separator
                     & Unit_To_File_Name
                       (Stub_Project_Prefix & Current_Source_Infix & Proj))));

               if Arg_Proj.Is_Library then
                  Append
                    (F,
                     GNATCOLL.VFS.Create
                       (+(Arg_Proj.Stub_Dir.all
                        & Directory_Separator
                        & Unit_To_File_Name
                          (Stub_Project_Prefix
                           & Current_Source_Infix
                           & Proj
                           & "_lib"))));
               end if;
               Create_Dirs (F);
            end;

            Relative_P_Path := new String'
              (+Relative_Path
                 (Create (+Arg_Proj.Path.all),
                  Create (+Arg_Proj.Stub_Dir.all)));

            Trace
              (Me,
               "Creating "
               & Arg_Proj.Stub_Dir.all
               & Directory_Separator
               & Unit_To_File_Name
                 (Stub_Project_Prefix & Current_Source_Infix & Proj)
               & ".gpr");
            Create
              (Arg_Proj.Stub_Dir.all
               & Directory_Separator
               & Unit_To_File_Name
                 (Stub_Project_Prefix & Current_Source_Infix & Proj)
               & ".gpr");

            I_Cur := Arg_Proj.Imported_List.First;
            while I_Cur /= List_Of_Strings.No_Element loop
               if
                 PF_Table.Element
                 (List_Of_Strings.Element (I_Cur)).Needed_For_Extention
               then
                  declare
                     Imported_Sub_Project : constant String :=
                       PF_Table.Element
                         (List_Of_Strings.Element (I_Cur)).Stub_Dir.all
                         & Directory_Separator
                       & To_Lower (Stub_Project_Prefix
                                   & Current_Source_Infix
                                   & List_Of_Strings.Element (I_Cur))
                       & ".gpr";
                  begin
                     if List_Of_Strings.Element (I_Cur) = Subroot_Prj_Name then
                        Relative_I_Path := new String'
                          (+Relative_Path (Create (+Subroot_Stub_Prj),
                           Create (+Arg_Proj.Stub_Dir.all)));
                     else
                        Relative_I_Path := new String'
                          (+Relative_Path (Create (+Imported_Sub_Project),
                           Create (+Arg_Proj.Stub_Dir.all)));
                     end if;
                  end;
                  if Arg_Proj.Limited_Withed.Contains
                    (List_Of_Strings.Element (I_Cur))
                  then
                     S_Put
                       (0,
                        "limited with """
                        & Relative_I_Path.all
                        & """;");
                  else
                     S_Put
                       (0,
                        "with """
                        & Relative_I_Path.all
                        & """;");
                  end if;
                  Put_New_Line;
               end if;
               Next (I_Cur);
            end loop;

            S_Put (0, "with ""aunit"";");
            Put_New_Line;
            Put_New_Line;

            S_Put
              (0,
               "project "
               & Stub_Project_Prefix
               & Current_Source_Infix
               & Proj
               & " extends """
               & Relative_P_Path.all
               & """ is");
            Put_New_Line;
            S_Put (3, "for Source_Dirs use (""."");");
            Put_New_Line;

            Set_Present_Subset_For_Project (Proj);
            E_Cur := Current_Proj_Present_Sources.First;
            if E_Cur /= String_Set.No_Element then
               S_Put (3, "for Source_Files use (");
               Put_New_Line;
            else
               S_Put (3, "for Source_Files use ();");
               Put_New_Line;
            end if;
            while E_Cur /= String_Set.No_Element loop
               if not Excluded_Test_Data_Files.Contains
                 (Get_Source_Stub_Data_Spec (String_Set.Element (E_Cur)))
               then
                  S_Put
                    (6,
                     """"
                     & Base_Name
                       (Get_Source_Stub_Data_Spec (String_Set.Element (E_Cur)))
                     & """,");
                  Put_New_Line;
               end if;
               if not Excluded_Test_Data_Files.Contains
                 (Get_Source_Stub_Data_Body (String_Set.Element (E_Cur)))
               then
                  S_Put
                    (6,
                     """"
                     & Base_Name
                       (Get_Source_Stub_Data_Body (String_Set.Element (E_Cur)))
                     & """,");
                  Put_New_Line;
               end if;
               S_Put
                 (6,
                  """"
                  & Base_Name (Get_Source_Body (String_Set.Element (E_Cur)))
                  & """");
               Next (E_Cur);
               if E_Cur = String_Set.No_Element then
                  S_Put (0, ");");
               else
                  S_Put (0, ",");
               end if;
               Put_New_Line;
            end loop;

            S_Put
              (3,
               "for Object_Dir use """
               & Unit_To_File_Name
                 (Stub_Project_Prefix & Current_Source_Infix & Proj)
               & """;");
            Put_New_Line;
            if Arg_Proj.Is_Library then
               S_Put
                 (3,
                  "for Library_Dir use """
                  & Unit_To_File_Name
                    (Stub_Project_Prefix
                     & Current_Source_Infix & Proj & "_lib")
                  & """;");
               Put_New_Line;
               S_Put
                 (3,
                  "for Library_Name use """
                  & Unit_To_File_Name
                    (Stub_Project_Prefix & Current_Source_Infix & Proj)
                  & """;");
               Put_New_Line;
            end if;
            Put_New_Line;

            E_Cur := Current_Proj_Present_Sources.First;
            if E_Cur /= String_Set.No_Element then
               S_Put (3, "package Coverage is");
               Put_New_Line;
               S_Put (6, "for Excluded_Units use (");
               Put_New_Line;

               while E_Cur /= String_Set.No_Element loop
                  S_Put
                    (9,
                     """"
                     & Get_Source_Unit_Name
                       (Get_Source_Body (String_Set.Element (E_Cur)))
                     & """");
                  Next (E_Cur);
                  if E_Cur = String_Set.No_Element then
                     S_Put (0, ");");
                  else
                     S_Put (0, ",");
                  end if;
                  Put_New_Line;
               end loop;
               S_Put (3, "end Coverage;");
               Put_New_Line;
            end if;

            S_Put
              (0,
               "end "
               & Stub_Project_Prefix
               & Current_Source_Infix
               & Proj
               & ";");

            Close_File;
         end if;

         <<Process_Imported>>

         Cur := Arg_Proj.Imported_List.First;
         while Cur /= List_Of_Strings.No_Element loop
            Process_Project (List_Of_Strings.Element (Cur));
            Next (Cur);
         end loop;
      end Process_Project;

      procedure Set_Present_Subset_For_Project (Proj : String) is
         Cur : Source_File_Table.Cursor := SF_Table.First;
      begin
         Current_Proj_Present_Sources.Clear;

         while Cur /= Source_File_Table.No_Element loop
            declare
               Key  : constant String := Source_File_Table.Key (Cur);
            begin
               if Source_File_Table.Element (Cur).Project_Name.all = Proj
                 and then not Is_Body (Key)
                 and then Source_Stubbed (Key)
                 and then not Excluded_Sources.Contains (Base_Name (Key))
               then
                  Current_Proj_Present_Sources.Include
                    (Source_File_Table.Key (Cur));
               end if;
            end;
            Next (Cur);
         end loop;
      end Set_Present_Subset_For_Project;
   begin
      Union (Excluded_Sources, Default_Stub_Exclusion_List);
      if Stub_Exclusion_Lists.Contains (Short_Name) then
         Union (Excluded_Sources, Stub_Exclusion_Lists.Element (Short_Name));
      end if;

      if Excluded_Sources.Is_Empty then
         Trace
           (Me,
            "No special extending project subtree needed for" & Short_Name);
         return;
      end if;

      Trace
        (Me, "Creating extending project subtree for source " & Short_Name);

      if Me_Verbose.Is_Active then
         Trace (Me_Verbose, "Current infix is " & Current_Source_Infix);
         Trace (Me_Verbose, "Root of subtree is " & Subroot_Prj_Name);
         Trace (Me_Verbose, "excluded sources are:");
         Increase_Indent (Me_Verbose);
         SS_Cur := Excluded_Sources.First;
            while SS_Cur /= String_Set.No_Element loop
               Trace (Me_Verbose, String_Set.Element (SS_Cur));
               Next (SS_Cur);
            end loop;
         Decrease_Indent (Me_Verbose);
      end if;

      Process_Project (Subroot_Prj_Name);

   end Enforce_Custom_Project_Extention;

   -------------------------------
   -- Enforce_Project_Extention --
   -------------------------------

   procedure Enforce_Project_Extention
     (Prj_Name              : String;
      Subroot_Stub_Prj      : String;
      Current_Project_Infix : String)
   is

      Processed_Projects : String_Set.Set := String_Set.Empty_Set;

      Current_Proj_Present_Sources : String_Set.Set := String_Set.Empty_Set;

      procedure Process_Project (Proj : String);
      procedure Set_Present_Subset_For_Project (Proj : String);

      procedure Set_Present_Subset_For_Project (Proj : String) is
         Cur : Source_File_Table.Cursor := SF_Table.First;
      begin
         Current_Proj_Present_Sources.Clear;

         while Cur /= Source_File_Table.No_Element loop
            declare
               Key  : constant String := Source_File_Table.Key (Cur);
            begin
               if Source_File_Table.Element (Cur).Project_Name.all = Proj
                 and then not Is_Body (Key)
                 and then Source_Stubbed (Key)
                 and then not
                   Default_Stub_Exclusion_List.Contains (Base_Name (Key))
               then
                  Current_Proj_Present_Sources.Include
                    (Source_File_Table.Key (Cur));
               end if;
            end;
            Next (Cur);
         end loop;
      end Set_Present_Subset_For_Project;

      procedure Process_Project (Proj : String) is
         Relative_P_Path, Relative_I_Path : String_Access;
         Arg_Proj : Project_Record;
         Cur, I_Cur : List_Of_Strings.Cursor;
         E_Cur : String_Set.Cursor;
      begin
         if Processed_Projects.Contains (Proj) then
            return;
         end if;
         Processed_Projects.Include (Proj);

         Arg_Proj := PF_Table.Element (Proj);

         if Proj = Prj_Name then
            --  The root of the subtree is extended by the test driver project.
            goto Process_Imported;
         end if;

         --  generating stuff
         if Arg_Proj.Needed_For_Extention then

            declare
               F : File_Array_Access;
            begin
               Append
                 (F,
                  GNATCOLL.VFS.Create
                    (+(Arg_Proj.Stub_Dir.all)));
               Append
                 (F,
                  GNATCOLL.VFS.Create
                    (+(Arg_Proj.Stub_Dir.all
                     & Directory_Separator
                     & Unit_To_File_Name
                       (Stub_Project_Prefix & Current_Project_Infix & Proj))));
               if Arg_Proj.Is_Library then
                  Append
                    (F,
                     GNATCOLL.VFS.Create
                       (+(Arg_Proj.Stub_Dir.all
                        & Directory_Separator
                        & Unit_To_File_Name
                          (Stub_Project_Prefix
                           & Current_Project_Infix
                           & Proj
                           & "_lib"))));
               end if;
               Create_Dirs (F);
            end;

            Relative_P_Path := new String'
              (+Relative_Path
                 (Create (+Arg_Proj.Path.all),
                  Create (+Arg_Proj.Stub_Dir.all)));

            Trace
              (Me,
               "Creating "
               & Arg_Proj.Stub_Dir.all
               & Directory_Separator
               & Unit_To_File_Name
                 (Stub_Project_Prefix & Current_Project_Infix & Proj)
               & ".gpr");
            Create
              (Arg_Proj.Stub_Dir.all
               & Directory_Separator
               & Unit_To_File_Name
                 (Stub_Project_Prefix & Current_Project_Infix & Proj)
               & ".gpr");

            I_Cur := Arg_Proj.Imported_List.First;
            while I_Cur /= List_Of_Strings.No_Element loop
               if
                 PF_Table.Element
                 (List_Of_Strings.Element (I_Cur)).Needed_For_Extention
               then
                  declare
                     Imported_Sub_Project : constant String :=
                       PF_Table.Element
                         (List_Of_Strings.Element (I_Cur)).Stub_Dir.all
                         & Directory_Separator
                       & To_Lower (Stub_Project_Prefix
                                   & Current_Project_Infix
                                   & List_Of_Strings.Element (I_Cur))
                       & ".gpr";
                  begin
                     if List_Of_Strings.Element (I_Cur) = Prj_Name then
                        Relative_I_Path := new String'
                          (+Relative_Path (Create (+Subroot_Stub_Prj),
                           Create (+Arg_Proj.Stub_Dir.all)));
                     else
                        Relative_I_Path := new String'
                          (+Relative_Path (Create (+Imported_Sub_Project),
                           Create (+Arg_Proj.Stub_Dir.all)));
                     end if;
                  end;
                  if Arg_Proj.Limited_Withed.Contains
                    (List_Of_Strings.Element (I_Cur))
                  then
                     S_Put
                       (0,
                        "limited with """
                        & Relative_I_Path.all
                        & """;");
                  else
                     S_Put
                       (0,
                        "with """
                        & Relative_I_Path.all
                        & """;");
                  end if;
                  Put_New_Line;
               end if;
               Next (I_Cur);
            end loop;

            S_Put (0, "with ""aunit"";");
            Put_New_Line;
            Put_New_Line;

            S_Put
              (0,
               "project "
               & Stub_Project_Prefix
               & Current_Project_Infix
               & Proj
               & " extends """
               & Relative_P_Path.all
               & """ is");
            Put_New_Line;
            S_Put (3, "for Source_Dirs use (""."");");
            Put_New_Line;

            Set_Present_Subset_For_Project (Proj);
            E_Cur := Current_Proj_Present_Sources.First;
            if E_Cur /= String_Set.No_Element then
               S_Put (3, "for Source_Files use (");
               Put_New_Line;
            else
               S_Put (3, "for Source_Files use ();");
               Put_New_Line;
            end if;
            while E_Cur /= String_Set.No_Element loop
               if not Excluded_Test_Data_Files.Contains
                 (Get_Source_Stub_Data_Spec (String_Set.Element (E_Cur)))
               then
                  S_Put
                    (6,
                     """"
                     & Base_Name
                       (Get_Source_Stub_Data_Spec (String_Set.Element (E_Cur)))
                     & """,");
                  Put_New_Line;
               end if;
               if not Excluded_Test_Data_Files.Contains
                 (Get_Source_Stub_Data_Body (String_Set.Element (E_Cur)))
               then
                  S_Put
                    (6,
                     """"
                     & Base_Name
                       (Get_Source_Stub_Data_Body (String_Set.Element (E_Cur)))
                     & """,");
                  Put_New_Line;
               end if;
               S_Put
                 (6,
                  """"
                  & Base_Name (Get_Source_Body (String_Set.Element (E_Cur)))
                  & """");
               Next (E_Cur);
               if E_Cur = String_Set.No_Element then
                  S_Put (0, ");");
               else
                  S_Put (0, ",");
               end if;
               Put_New_Line;
            end loop;

            S_Put
              (3,
               "for Object_Dir use """
               & Unit_To_File_Name
                 (Stub_Project_Prefix & Current_Project_Infix & Proj)
               & """;");
            Put_New_Line;
            if Arg_Proj.Is_Library then
               S_Put
                 (3,
                  "for Library_Dir use """
                  & Unit_To_File_Name
                    (Stub_Project_Prefix
                     & Current_Project_Infix & Proj & "_lib")
                  & """;");
               Put_New_Line;
               S_Put
                 (3,
                  "for Library_Name use """
                  & Unit_To_File_Name
                    (Stub_Project_Prefix & Current_Project_Infix & Proj)
                  & """;");
               Put_New_Line;
            end if;
            Put_New_Line;

            E_Cur := Current_Proj_Present_Sources.First;
            if E_Cur /= String_Set.No_Element then
               S_Put (3, "package Coverage is");
               Put_New_Line;
               S_Put (6, "for Excluded_Units use (");
               Put_New_Line;

               while E_Cur /= String_Set.No_Element loop
                  S_Put
                    (9,
                     """"
                     & Get_Source_Unit_Name
                       (Get_Source_Body (String_Set.Element (E_Cur)))
                     & """");
                  Next (E_Cur);
                  if E_Cur = String_Set.No_Element then
                     S_Put (0, ");");
                  else
                     S_Put (0, ",");
                  end if;
                  Put_New_Line;
               end loop;
               S_Put (3, "end Coverage;");
               Put_New_Line;
            end if;

            S_Put
              (0,
               "end "
               & Stub_Project_Prefix
               & Current_Project_Infix
               & Proj
               & ";");

            Close_File;
         end if;

         <<Process_Imported>>

         Cur := Arg_Proj.Imported_List.First;
         while Cur /= List_Of_Strings.No_Element loop
            Process_Project (List_Of_Strings.Element (Cur));
            Next (Cur);
         end loop;
      end Process_Project;

   begin

      Process_Project (Prj_Name);

   end Enforce_Project_Extention;

end Test.Skeleton.Source_Table;

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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Test.Common;            use Test.Common;

package body Test.Harness.Source_Table is

   -----------------------
   -- Source File table --
   -----------------------

   type SF_Record;

   Sources_Total : Natural := 1;

   type SF_Record is record

      Full_Source_Name : String_Access;
      --  This field stores the source name with full directory information
      --  in absolute form.

      Suffixless_Name : String_Access;
      --  The source name without directory information and suffix (if any)
      --  is used to create the names of the tree file and ALI files.

      Status : SF_Status;
      --  Status of the given source. Initially is set to Waiting, then is
      --  changed according to the results of the metrics computation.
   end record;

   package Source_File_Table is new
     Ada.Containers.Indefinite_Ordered_Maps (String, SF_Record);

   package Source_File_Locations is new
     Ada.Containers.Indefinite_Ordered_Sets (String);

   use Source_File_Table;
   use Source_File_Locations;

   SF_Table : Source_File_Table.Map;
   --  Source Table itself

   SFL_Table : Source_File_Locations.Set;
   --  A set of paths to source files. Used for creation of project file.

   SF_Iterator  : Source_File_Table.Cursor;
   SFL_Iterator : Source_File_Locations.Cursor;

   Short_Source_Name_String : String_Access;
   Full_Source_Name_String  : String_Access;

   procedure Reset_Source_Iterator;
   --  Sets SF_Iterator to the begining of SF_Table

   ---------------------------
   -- Add_Source_To_Process --
   ---------------------------

   procedure Add_Source_To_Process (Fname : String) is
      First_Idx : Natural;
      Last_Idx  : Natural;

      New_SF_Record : SF_Record;
   begin
      if not Is_Regular_File (Fname) then
         Report_Std ("gnattest: " & Fname & " not found");
         return;
      end if;

      --  Check if we already have a file with the same short name
      Short_Source_Name_String := new String'(Base_Name (Fname));
      Full_Source_Name_String  :=
        new String'(Normalize_Pathname
          (Fname,
           Resolve_Links  => False,
             Case_Sensitive => False));

      SF_Iterator := Find (SF_Table, Short_Source_Name_String.all);

      if not (SF_Iterator = Source_File_Table.No_Element) then
         Report_Std ("gnattest: more than one version of "
                     & Short_Source_Name_String.all & " processed");
         Report_Std
           ("multiple files sharing the same name aren't supported", 10);
         Report_Std (Full_Source_Name_String.all & " is ignored", 10);
         return;
      end if;

      --  If we are here, we have to store the file in the table

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

      Insert (SF_Table, Short_Source_Name_String.all, New_SF_Record);

      Include
        (SFL_Table,
         Normalize_Pathname (Name => Dir_Name (Full_Source_Name_String.all),
                             Case_Sensitive => False));

      Sources_Total := Sources_Total + 1;

      Free (Short_Source_Name_String);
      Free (Full_Source_Name_String);

   end Add_Source_To_Process;

   --------------
   -- Is_Empty --
   --------------

   function SF_Table_Empty return Boolean is
   begin
      return Is_Empty (SF_Table);
   end SF_Table_Empty;

   --------------------------
   -- Get_Source_Full_Name --
   --------------------------

   function Get_Source_Full_Name (Source_Name : String) return String is
   begin
      return Source_File_Table.Element
        (SF_Table, Source_Name).Full_Source_Name.all;
   end Get_Source_Full_Name;

   -----------------------
   -- Get_Source_Status --
   -----------------------

   function Get_Source_Status (Source_Name : String) return SF_Status is
   begin
      return Source_File_Table.Element
        (SF_Table, Source_Name).Status;
   end Get_Source_Status;

   --------------------------------
   -- Get_Source_Suffixless_Name --
   --------------------------------

   function Get_Source_Suffixless_Name (Source_Name : String) return String is
   begin
      return Source_File_Table.Element
        (SF_Table, Source_Name).Suffixless_Name.all;
   end Get_Source_Suffixless_Name;

   -------------------------------
   -- Next_Non_Processed_Source --
   -------------------------------

   function Next_Non_Processed_Source return String is
      Cur_Pending : Source_File_Table.Cursor := Source_File_Table.No_Element;
      Cur_Body    : Source_File_Table.Cursor := Source_File_Table.No_Element;
   begin
      Reset_Source_Iterator;

      loop
         if Cur_Pending = Source_File_Table.No_Element and then
           Source_File_Table.Element (SF_Iterator).Status = Pending
         then
            Cur_Pending := SF_Iterator;
         end if;

         if Cur_Pending = Source_File_Table.No_Element and then
           Source_File_Table.Element (SF_Iterator).Status = Pending_For_Body
         then
            Cur_Body := SF_Iterator;
         end if;

         if Source_File_Table.Element (SF_Iterator).Status = Waiting then
            return Key (SF_Iterator);
         end if;

         Next (SF_Iterator);
         exit when SF_Iterator = Source_File_Table.No_Element;
      end loop;

      if Cur_Pending /= Source_File_Table.No_Element then
         return Key (Cur_Pending);
      end if;

      if Cur_Body /= Source_File_Table.No_Element then
         return Key (Cur_Body);
      end if;

      return "";
   end Next_Non_Processed_Source;

   --------------------------
   -- Next_Source_Location --
   --------------------------

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

   -----------------------------
   -- Reset_Location_Iterator --
   -----------------------------
   procedure Reset_Location_Iterator is
   begin
      SFL_Iterator := First (SFL_Table);
   end Reset_Location_Iterator;

   ---------------------------
   -- Reset_Source_Iterator --
   ---------------------------

   procedure Reset_Source_Iterator is
   begin
      SF_Iterator := First (SF_Table);
   end Reset_Source_Iterator;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Source_Status (Source_Name : String;
                                New_Status : SF_Status) is
      SF_Rec : SF_Record;
   begin
      SF_Rec := Source_File_Table.Element (SF_Table, Source_Name);
      SF_Rec.Status := New_Status;
      Replace (SF_Table, Source_Name, SF_Rec);
   end Set_Source_Status;

   --------------------
   -- Source_Present --
   --------------------

   function Source_Present (Source_Name : String) return Boolean is
   begin
      return Contains (SF_Table, Source_Name);
   end Source_Present;

end Test.Harness.Source_Table;

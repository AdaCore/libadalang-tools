------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2015-2021, AdaCore                    --
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

with Test.Common;            use Test.Common;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

package body Test.Mapping is

   Me : constant Trace_Handle := Create ("Mapping", Default => Off);

   New_Line_Counter_Val : Natural;

   Harness_Dir : GNAT.OS_Lib.String_Access renames Harness_Dir_Str;

   -------------------
   -- Add_Stub_List --
   -------------------

   procedure Add_Test_List (Name : String; List : TP_Mapping_List.List) is
      M : Mapping_Type;
   begin
      Trace (Me, "adding test list for " & Name);
      if Mapping.Contains (Name) then
         M := Mapping.Element (Name);
         M.Test_Info := Copy (List);
         Mapping.Replace (Name, M);
      else
         M.Test_Info := Copy (List);
         Mapping.Include (Name, M);
      end if;
   end Add_Test_List;

   -------------------
   -- Add_Stub_List --
   -------------------

   procedure Add_Stub_List (Name : String; Info : Stub_Unit_Mapping)
   is
      M : Mapping_Type;
   begin
      Trace (Me, "adding stub list for " & Name);
      if Mapping.Contains (Name) then
         M := Mapping.Element (Name);
         Clone (Info, M.Stub_Info);
         Mapping.Replace (Name, M);
      else
         Clone (Info, M.Stub_Info);
         Mapping.Include (Name, M);
      end if;
   end Add_Stub_List;

   -----------
   -- Clone --
   -----------

   procedure Clone (From : Stub_Unit_Mapping; To : in out Stub_Unit_Mapping) is
   begin
      To.Stub_Data_File_Name := new String'(From.Stub_Data_File_Name.all);
      To.Orig_Body_File_Name := new String'(From.Orig_Body_File_Name.all);
      To.Stub_Body_File_Name := new String'(From.Stub_Body_File_Name.all);
      To.Entities := Copy (From.Entities);
      To.D_Bodies := Copy (From.D_Bodies);
      To.D_Setters := Copy (From.D_Setters);
   end Clone;

   ---------------------------
   -- Generate_Mapping_File --
   ---------------------------

   procedure Generate_Mapping_File is
      TC : TC_Mapping;
      TR : TR_Mapping;
      TP : TP_Mapping;
      DT : DT_Mapping;
      TP_List : TP_Mapping_List.List;

      TC_Cur : TC_Mapping_List.Cursor;
      TR_Cur : TR_Mapping_List.Cursor;
      TP_Cur : TP_Mapping_List.Cursor;
      SP_Cur : SP_Mapping.Cursor;
      DT_Cur : DT_Mapping_List.Cursor;

      SI : Stub_Unit_Mapping;
      ES : Entity_Stub_Mapping;
      ES_Cur : Entity_Stub_Mapping_List.Cursor;
      Sloc_Cur : ES_List.Cursor;

      UTP : User_Test_Package;
      UTP_Cur : UTP_Mapping_List.Cursor;

      function Get_Path_Relative_To_XML (Path : String) return String is
         (+Relative_Path (Create (+Path), Create (+Harness_Dir.all)));
   begin
      Trace (Me, "generating mapping file");
      Create (Harness_Dir.all &
              Directory_Separator &
              "gnattest.xml");

      if Generate_Separates then
         S_Put (0, "<tests_mapping mode=""separates"">");
      else
         S_Put (0, "<tests_mapping mode=""monolith"">");
      end if;
      Put_New_Line;

      SP_Cur := Mapping.First;
      loop
         exit when SP_Cur = SP_Mapping.No_Element;

         S_Put
           (3,
            "<unit source_file=""" &
            Base_Name (SP_Mapping.Key (SP_Cur)) &
            """>");
         Put_New_Line;

         TP_List := SP_Mapping.Element (SP_Cur).Test_Info;
         if TP_List /= TP_Mapping_List.Empty_List then
            TP_Cur := TP_List.First;
            loop
               exit when TP_Cur = TP_Mapping_List.No_Element;

               TP := TP_Mapping_List.Element (TP_Cur);

               S_Put
                 (6,
                  "<test_unit target_file=""" &
                    TP.TP_Name.all &
                    """>");
               Put_New_Line;

               if TP.SetUp_Name /= null then
                  S_Put
                    (9,
                     "<setup file=""" &
                       TP.SetUp_Name.all &
                       """ name=""Set_Up"" line=""" &
                       Trim (Natural'Image (TP.SetUp_Line), Both) &
                       """  column=""" &
                       Trim (Natural'Image (TP.SetUp_Column), Both) &
                       """/>");
                  Put_New_Line;
                  S_Put
                    (9,
                     "<teardown file=""" &
                       TP.TearDown_Name.all &
                       """ name=""Tear_Down"" line=""" &
                       Trim (Natural'Image (TP.TearDown_Line), Both) &
                       """  column=""" &
                       Trim (Natural'Image (TP.TearDown_Column), Both) &
                       """/>");
                  Put_New_Line;
               end if;

               TR_Cur := TP.TR_List.First;
               loop
                  exit when TR_Cur = TR_Mapping_List.No_Element;

                  TR := TR_Mapping_List.Element (TR_Cur);

                  S_Put
                    (9,
                     "<tested name=""" &
                       TR.TR_Name.all &
                       """ line=""" &
                       Trim (Natural'Image (TR.Line), Both) &
                       """ column=""" &
                       Trim (Natural'Image (TR.Column), Both) &
                       """>");
                  Put_New_Line;

                  if TR.Test = null then

                     TC_Cur := TR.TC_List.First;
                     loop
                        exit when TC_Cur = TC_Mapping_List.No_Element;

                        TC := TC_Mapping_List.Element (TC_Cur);

                        S_Put
                          (12,
                           "<test_case name=""" &
                             TC.TC_Name.all &
                             """ line=""" &
                             Trim (Natural'Image (TC.Line), Both) &
                             """ column=""" &
                             Trim (Natural'Image (TC.Column), Both) &
                             """>");
                        Put_New_Line;
                        S_Put
                          (15,
                           "<test file="""
                           & TC.Test.all
                           & """ line="""
                           & Trim (Natural'Image (TC.TR_Line), Both)
                           & """ column=""1"""
                           & " name=""" & TC.T_Name.all & """");
                        S_Put (0, " timestamp="""
                               & TC.Test_Time.all
                               & """/>");
                        Put_New_Line;
                        S_Put (12, "</test_case>");
                        Put_New_Line;

                        TC_Mapping_List.Next (TC_Cur);
                     end loop;

                  else
                     S_Put
                       (12,
                        "<test file="""
                        & TR.Test.all
                        & """ line="""
                        & Trim (Natural'Image (TR.TR_Line), Both)
                        & """ column=""1"""
                        & " name=""" & TR.T_Name.all & """");
                     S_Put (0, " timestamp="""
                            & TR.Test_Time.all
                            & """/>");
                     Put_New_Line;
                  end if;

                  S_Put (9, "</tested>");
                  Put_New_Line;

                  TR_Mapping_List.Next (TR_Cur);
               end loop;

               S_Put (9, "<dangling>");
               Put_New_Line;
               DT_Cur := TP.DT_List.First;
               loop
                  exit when DT_Cur = DT_Mapping_List.No_Element;

                  DT := DT_Mapping_List.Element (DT_Cur);

                  S_Put
                    (12,
                     "<test file="""
                     & DT.File.all
                     & """ line="""
                     & Trim (Natural'Image (DT.Line), Both)
                     & """ column="""
                     & Trim (Natural'Image (DT.Column), Both)
                     & """/>");
                  Put_New_Line;

                  DT_Mapping_List.Next (DT_Cur);
               end loop;
               S_Put (9, "</dangling>");
               Put_New_Line;

               S_Put (6, "</test_unit>");
               Put_New_Line;

               TP_Mapping_List.Next (TP_Cur);
            end loop;
         end if;

         SI := SP_Mapping.Element (SP_Cur).Stub_Info;
         if SI /= Nil_Stub_Unit_Mapping then
            S_Put
              (6,
               "<stub_unit Original_body_file="""
               & Get_Path_Relative_To_XML (SI.Orig_Body_File_Name.all)
               & """ stub_body_file="""
               & Get_Path_Relative_To_XML (SI.Stub_Body_File_Name.all)
               & """ setter_file="""
               & Base_Name (SI.Stub_Data_File_Name.all)
               & """>");
            Put_New_Line;

            ES_Cur := SI.Entities.First;
            while ES_Cur /= Entity_Stub_Mapping_List.No_Element loop
               ES := Entity_Stub_Mapping_List.Element (ES_Cur);
               S_Put
                 (9,
                  "<stubbed name="""
                  & ES.Name.all
                  & """ line="""
                  & Trim (Natural'Image (ES.Line), Both)
                  & """ column="""
                  & Trim (Natural'Image (ES.Column), Both)
                  & """>");
               Put_New_Line;

               S_Put
                 (12,
                  "<stub_body line="""
                  & Trim (Natural'Image (ES.Stub_Body.Line), Both)
                  & """ column="""
                  & Trim (Natural'Image (ES.Stub_Body.Column), Both)
                  & """/>");
               Put_New_Line;
               if ES.Setter /= Nil_Entity_Sloc then
                  S_Put
                    (12,
                     "<setter line="""
                     & Trim (Natural'Image (ES.Setter.Line), Both)
                     & """ column="""
                     & Trim (Natural'Image (ES.Setter.Column), Both)
                     & """/>");
                  Put_New_Line;
               end if;

               S_Put (9, "</stubbed>");
               Put_New_Line;

               Next (ES_Cur);
            end loop;

            S_Put (9, "<dangling_stubs>");
            Put_New_Line;
            Sloc_Cur := SI.D_Bodies.First;
            while Sloc_Cur /= ES_List.No_Element loop
               S_Put
                 (12,
                  "<stub line="""
                  & Trim
                    (Natural'Image (ES_List.Element (Sloc_Cur).Line), Both)
                  & """ column="""
                  & Trim
                    (Natural'Image (ES_List.Element (Sloc_Cur).Column), Both)
                  & """/>");
               Put_New_Line;
               Next (Sloc_Cur);
            end loop;
            S_Put (9, "</dangling_stubs>");
            Put_New_Line;

            S_Put (9, "<dangling_setters>");
            Put_New_Line;
            Sloc_Cur := SI.D_Setters.First;
            while Sloc_Cur /= ES_List.No_Element loop
               S_Put
                 (12,
                  "<setter line="""
                  & Trim
                    (Natural'Image (ES_List.Element (Sloc_Cur).Line), Both)
                  & """ column="""
                  & Trim
                    (Natural'Image (ES_List.Element (Sloc_Cur).Column), Both)
                  & """/>");
               Put_New_Line;
               Next (Sloc_Cur);
            end loop;
            S_Put (9, "</dangling_setters>");
            Put_New_Line;

            S_Put (6, "</stub_unit>");
            Put_New_Line;
         end if;

         S_Put (3, "</unit>");
         Put_New_Line;

         SP_Mapping.Next (SP_Cur);
      end loop;

      if not Additional_Mapping.Is_Empty then
         S_Put (3, "<additional_tests>");
         Put_New_Line;

         UTP_Cur := Additional_Mapping.First;
         while UTP_Cur /= UTP_Mapping_List.No_Element loop
            UTP := UTP_Mapping_List.Element (UTP_Cur);

            S_Put (6, "<test_unit target_file=""" & UTP.Name.all & """>");
            Put_New_Line;
            S_Put
              (9,
               "<test_type name="""
               & UTP.Type_Name.all
               & """ line="""
               & Trim (UTP.Type_Sloc.Line'Img, Both)
               & """ column="""
               & Trim (UTP.Type_Sloc.Column'Img, Both)
               & """>");
            Put_New_Line;

            if UTP.SetUp_Sloc /= Nil_Entity_Sloc then
               S_Put
                 (12,
                  "<setup line="""
                  & Trim (UTP.SetUp_Sloc.Line'Img, Both)
                  & """ column="""
                  & Trim (UTP.SetUp_Sloc.Column'Img, Both)
                  & """/>");
            else
               S_Put (12, "<setup/>");
            end if;
            Put_New_Line;

            if UTP.TearDown_Sloc /= Nil_Entity_Sloc then
               S_Put
                 (12,
                  "<teardown line="""
                  & Trim (UTP.TearDown_Sloc.Line'Img, Both)
                  & """ column="""
                  & Trim (UTP.TearDown_Sloc.Column'Img, Both)
                  & """/>");
            else
               S_Put (12, "<teardown/>");
            end if;
            Put_New_Line;

            TR_Cur := UTP.TR_List.First;
            while TR_Cur /= TR_Mapping_List.No_Element loop
               TR := TR_Mapping_List.Element (TR_Cur);

               S_Put
                 (12,
                  "<test name="""
                  & TR.TR_Name.all
                  & """ line="""
                  & Trim (TR.Line'Img, Both)
                  & """ column="""
                  & Trim (TR.Column'Img, Both)
                  & """/>");
               Put_New_Line;

               Next (TR_Cur);
            end loop;

            S_Put (9, "</test_type>");
            Put_New_Line;
            S_Put (6, "</test_unit>");
            Put_New_Line;

            Next (UTP_Cur);
         end loop;

         S_Put (3, "</additional_tests>");
         Put_New_Line;
      else
         S_Put (3, "<additional_tests/>");
         Put_New_Line;
      end if;

      S_Put (0, "</tests_mapping>");

      Close_File;

      Mapping.Clear;
   end Generate_Mapping_File;

   ----------------------
   -- New_Line_Counter --
   ----------------------

   function New_Line_Counter return Natural is
   begin
      return New_Line_Counter_Val;
   end New_Line_Counter;

   ------------------------
   -- Reset_Line_Counter --
   ------------------------

   procedure Reset_Line_Counter is
   begin
      New_Line_Counter_Val := 1;
   end Reset_Line_Counter;

   ------------------------------
   -- procedure New_Line_Count --
   ------------------------------

   procedure New_Line_Count is
   begin
      New_Line_Counter_Val := New_Line_Counter_Val + 1;
      Put_New_Line;
   end New_Line_Count;

end Test.Mapping;

------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                     G N A T T E S T . M A P P I N G                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2015-2019, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Test.Mapping is

   -------------------
   -- Tests mapping --
   -------------------

   type TC_Mapping is record
      TC_Name   : String_Access;
      Line      : Natural;
      Column    : Natural;
      Test      : String_Access;
      Test_Time : String_Access;
      T_Name    : String_Access;
      --  Name of the test routine.

      TR_Line   : Natural;
      --  Only used in no separates mode.
   end record;
   --  Stores info on individual test cases of subprogram under test.

   package TC_Mapping_List is new
     Ada.Containers.Doubly_Linked_Lists (TC_Mapping);
   use TC_Mapping_List;

   type TR_Mapping is record
      TR_Name   : String_Access;
      --  Name of subprogram under test.
      T_Name   : String_Access;
      --  Name of the test routine.
      Line      : Natural;
      Column    : Natural;
      Test      : String_Access := null;
      Test_Time : String_Access := null;
      TC_List   : TC_Mapping_List.List;

      TR_Line   : Natural;
      --  Only used in no separates mode.
   end record;
   --  Stores info on individual subprogram under test and collection of
   --  corresponding test cases (if any).

   package TR_Mapping_List is new
     Ada.Containers.Doubly_Linked_Lists (TR_Mapping);
   use TR_Mapping_List;

   type DT_Mapping is record
      File   : String_Access;
      Line   : Natural;
      Column : Natural;
   end record;
   --  Stores info on individual dangling test.

   package DT_Mapping_List is new
     Ada.Containers.Doubly_Linked_Lists (DT_Mapping);
   use DT_Mapping_List;

   type TP_Mapping is record
      TP_Name         : String_Access;
      SetUp_Name      : String_Access;
      SetUp_Line      : Natural;
      SetUp_Column    : Natural;
      TearDown_Name   : String_Access;
      TearDown_Line   : Natural;
      TearDown_Column : Natural;
      TR_List         : TR_Mapping_List.List;
      DT_List         : DT_Mapping_List.List;
   end record;
   --  Stores info on individual test package, all it's test routines and
   --  dangling tests.

   package TP_Mapping_List is new
     Ada.Containers.Doubly_Linked_Lists (TP_Mapping);
   use TP_Mapping_List;

   -------------------
   -- Stubs mapping --
   -------------------

   type Entity_Sloc is record
      Line      : Natural;
      Column    : Natural;
   end record;
   --  Sloc info

   Nil_Entity_Sloc : constant Entity_Sloc := (0, 0);

   package ES_List is new
     Ada.Containers.Doubly_Linked_Lists (Entity_Sloc);
   use ES_List;

   type Entity_Stub_Mapping is record
      Name      : String_Access;
      Line      : Natural;
      Column    : Natural;

      Stub_Body     : Entity_Sloc;
      Setter        : Entity_Sloc;
   end record;
   --  Mapping info of entity from stubbed unit.

   function "=" (L, R : Entity_Stub_Mapping) return Boolean is
     (L.Name.all = R.Name.all and then L.Line = R.Line
      and then L.Column = R.Column);

   package Entity_Stub_Mapping_List is new
     Ada.Containers.Doubly_Linked_Lists (Entity_Stub_Mapping);
   use Entity_Stub_Mapping_List;

   type Stub_Unit_Mapping is record
      Stub_Data_File_Name : String_Access;
      Orig_Body_File_Name : String_Access;
      Stub_Body_File_Name : String_Access;
      Entities  : Entity_Stub_Mapping_List.List;
      D_Setters : ES_List.List;
      D_Bodies  : ES_List.List;
   end record;
   --  Mapping info for a whole stubbed unit.

   Nil_Stub_Unit_Mapping : constant Stub_Unit_Mapping :=
     (null, null, null,
      Entity_Stub_Mapping_List.Empty_List,
      ES_List.Empty_List, ES_List.Empty_List);

   procedure Clone (From : Stub_Unit_Mapping; To : in out Stub_Unit_Mapping);

   ------------------------
   -- User Tests Mapping --
   ------------------------

   type User_Test_Package is record
      Name          : String_Access;
      Type_Name     : String_Access;
      Type_Sloc     : Entity_Sloc := Nil_Entity_Sloc;
      SetUp_Sloc    : Entity_Sloc := Nil_Entity_Sloc;
      TearDown_Sloc : Entity_Sloc := Nil_Entity_Sloc;
      TR_List       : TR_Mapping_List.List := TR_Mapping_List.Empty_List;
   end record;

   package UTP_Mapping_List is new
     Ada.Containers.Doubly_Linked_Lists (User_Test_Package);
   use UTP_Mapping_List;

   type Mapping_Type is record
      Test_Info : TP_Mapping_List.List  := TP_Mapping_List.Empty_List;
      Stub_Info : Stub_Unit_Mapping    := Nil_Stub_Unit_Mapping;
   end record;

   package SP_Mapping is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Mapping_Type);
   use SP_Mapping;

   Mapping            : SP_Mapping.Map;
   Additional_Mapping : UTP_Mapping_List.List := UTP_Mapping_List.Empty_List;

   procedure Add_Test_List (Name : String; List : TP_Mapping_List.List);
   procedure Add_Stub_List (Name : String; Info : Stub_Unit_Mapping);
   --  Add test/stub mapping info for given unit.

   function New_Line_Counter return Natural;
   --  Returns current value of the counter

   procedure Reset_Line_Counter;
   --  Resets the counter to 1.

   procedure New_Line_Count;
   --  Wrapper that increases the counter of new lines in generated package.

   procedure Generate_Mapping_File;
   --  Creates a mapping file for tested suprograms and tests.

end Test.Mapping;

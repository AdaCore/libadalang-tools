------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2019-2022, AdaCore                    --
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

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
package Test.Command_Lines is

   package Test_Common_Nat_Shorthands is new Common_Nat_Switches
     .Set_Shorthands
       ([Jobs => +"--queues"]);

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   type Test_Booleans is
     (Strict,
      Recursive,
      Harness_Only,
      Stub,
      Validate_Type_Extensions,
      Inheritance_Check,
      Test_Case_Only,
      Omit_Sloc,
      Command_Line_Support,
      Test_Duration,
      Relocatable_Harness,
      Gen_Test_Vectors);

   package Test_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Test_Booleans);

   package Test_Boolean_Shorthands is new Test_Boolean_Switches
     .Set_Shorthands
     ([Recursive => +"-r",
       Command_Line_Support => +"--command-line",
       others => null]);

   --  Re: the --command-line/--no-command-line switch. We don't want an
   --  enumeration literal Command_Line here, because it causes conflicts
   --  with the type of the same name. So we call it Command_Line_Support,
   --  and add --command-line as a shorthand.

   package Test_Boolean_Defaults is new
     Test_Boolean_Switches.Set_Defaults
       ([Inheritance_Check => True,
         Command_Line_Support => True,
         Harness_Only => False,
         others => False]);

   type Test_Strings is
     (Separate_Drivers,
      Harness_Dir,
      Tests_Dir,
      Tests_Root,
      Stubs_Dir,
      Additional_Tests,
      Skeleton_Default,
      Passed_Tests,
      Exit_Status,
      Copy_Environment,
      Reporter,
      Gen_Test_Num,
      Gen_Unsupported_Behavior);

   package Test_String_Switches is new String_Switches
     (Descriptor,
      Test_Strings);

   package Test_String_Syntax is new Test_String_Switches.Set_Syntax
     ([Separate_Drivers         => '?',
       Harness_Dir              => '=',
       Tests_Dir                => '=',
       Tests_Root               => '=',
       Stubs_Dir                => '=',
       Additional_Tests         => '=',
       Skeleton_Default         => '=',
       Passed_Tests             => '=',
       Exit_Status              => '=',
       Copy_Environment         => '=',
       Reporter                 => '=',
       Gen_Test_Num             => '=',
       Gen_Unsupported_Behavior => '=']);

   type Test_String_Seqs is (Exclude_From_Stubbing);

   package Test_String_Seq_Switches is new String_Seq_Switches
     (Descriptor,
      Test_String_Seqs);

   package Test_String_Seq_Syntax is new Test_String_Seq_Switches
     .Set_Syntax
     ([Exclude_From_Stubbing => '!']);

   package Freeze is new Freeze_Descriptor (Descriptor);

end Test.Command_Lines;

------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--               G N A T T E S T . C O M M A N D _ L I N E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
package Test.Command_Lines is

   package Test_Common_Nat_Shorthands is new Common_Nat_Switches
     .Set_Shorthands
       ((Jobs => +"--queues"));

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
      Separates,
      Transition,
      Test_Duration);

   package Test_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Test_Booleans);

   package Test_Boolean_Shorthands is new Test_Boolean_Switches
     .Set_Shorthands
     ((Recursive => +"-r",
       Command_Line_Support => +"--command-line",
       others => null));

   --  Re: the --command-line/--no-command-line switch. We don't want an
   --  enumeration literal Command_Line here, because it causes conflicts
   --  with the type of the same name. So we call it Command_Line_Support,
   --  and add --command-line as a shorthand.

   package Test_Boolean_Defaults is new
     Test_Boolean_Switches.Set_Defaults
       ((Inheritance_Check => True,
         Command_Line_Support => True,
         Harness_Only => False,
         others => False));

   type Test_Strings is
     (Separate_Drivers,
      Harness_Dir,
      Tests_Dir,
      Subdir,
      Tests_Root,
      Stubs_Dir,
      Additional_Tests,
      Skeleton_Default,
      Passed_Tests,
      Exit_Status,
      Copy_Environment);

   package Test_String_Switches is new String_Switches
     (Descriptor,
      Test_Strings);

   type Test_String_Seqs is (Exclude_From_Stubbing);

   package Test_String_Seq_Switches is new String_Seq_Switches
     (Descriptor,
      Test_String_Seqs);

   package Test_String_Seq_Syntax is new Test_String_Seq_Switches
     .Set_Syntax
     ((Exclude_From_Stubbing => '!'));

   package Freeze is new Freeze_Descriptor (Descriptor);

end Test.Command_Lines;

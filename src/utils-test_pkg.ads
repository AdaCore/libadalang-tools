------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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
package Utils.Test_Pkg is

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   use Common_String_Seq_Switches;

   package Disable is new Disable_Switches (Descriptor, [To_All (Debug)]);

   type Some_Flags is (Do_This, Do_That, Do_The_Other_Thing);
   package Some_Flags_Switches is new Flag_Switches (Descriptor, Some_Flags);

   type Some_Booleans is (Syntax_Check, Code_Gen);
   package Some_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Some_Booleans);
   package Some_Boolean_Defaults is new Some_Boolean_Switches.Set_Defaults
     ([Syntax_Check => False, Code_Gen => True]);

   type My_Enum is (Th, This, That, The_Other_Thing);
   package My_Enum_Switches is new Enum_Switches (Descriptor, My_Enum);

   type Test_Debug_Switch is (Test_Debug);
   package My_Test_Debug_Switches is new String_Seq_Switches
     (Descriptor,
      Test_Debug_Switch);

   package Test_Debug_Options is new My_Test_Debug_Switches.Set_Syntax
     ([Test_Debug => '!']);

   package Test_Debug_Shorthands is new My_Test_Debug_Switches.Set_Shorthands
     ([Test_Debug => +"-debug"]);

   package Freeze is new Freeze_Descriptor (Descriptor);

end Utils.Test_Pkg;

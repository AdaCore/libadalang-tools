------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2009-2021, AdaCore                    --
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

package body Utils_Debug is

   ---------------------------------
   -- Summary of Debug Flag Usage --
   ---------------------------------

   --  da  not used yet
   --  db  not used yet
   --  dc  output calls to other executables issued by a tool
   --  dd  progress indicator mode
   --  de  not used yet
   --  df  not used yet
   --  dg  print out debug image of global structure (all nodes)
   --  dh  not used yet
   --  di  not used yet
   --  dj  not used yet
   --  dk  not used yet
   --  dl  last chance debug info
   --  dm  not used yet
   --  dn  keep temporary files
   --  do  not used yet
   --  dp  not used yet
   --  dq  not used yet
   --  dr  not used yet
   --  ds  print out debug image of source table (all sources)
   --  dt  print out list of units used to create a tree
   --  du  not used yet
   --  dv  verbose
   --  dw  not used yet
   --  dx  not used yet
   --  dy  not used yet
   --  dz  not used yet

   -----------------------------------
   -- Documentation for Debug Flags --
   -----------------------------------

   --  da   not used yet

   --  db   not used yet

   --  dc   print out the command used to generate the tree, and other calls
   --       to other executables issued by a tool,  with full switches and
   --       arguments

   --  dd   print out information for GPS progress indicator

   --  de   not used yet

   --  df   not used yet

   --  dg   print out debug image of global structure (all nodes)

   --  dh   not used yet

   --  di   not used yet

   --  dj   not used yet

   --  dk   not used yet

   --  dl   in the outermost exeption handlers, print out all the applicable
   --       debug information corresponding to other debug flags.

   --  dm   not used yet

   --  dn   do not delete the temporary working directory and all the generated
   --       temporary files (except tree and ALI files)

   --  do   not used yet

   --  dp   not used yet

   --  dq   not used yet

   --  dr   not used yet

   --  ds   print out debug image of source table (all sources - specifid as
   --       tool argument and added in the process of the tool run). Also print
   --       out all the file traces when filing in source table (with full
   --       absolute pathnames

   --  dt   print out the full list of Ada source files used to create the tree
   --       file for ASIS. Source file names contain full path information in
   --       absolure form.

   --  du   not used yet

   --  dv   Verbose. This differs from -v in that it avoids printing full path
   --       names, version numbers, and the like, so can be used in regression
   --       tests.

   --  dw   not used yet

   --  dx   not used yet

   --  dy   not used yet

   --  dz   not used yet

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True) is
      subtype Dig  is Character range '1' .. '9';
      subtype LLet is Character range 'a' .. 'z';

   begin
      if C in Dig then
         case Dig'(C) is
            when '1' => Debug_Flag_1 := Val;
            when '2' => Debug_Flag_2 := Val;
            when '3' => Debug_Flag_3 := Val;
            when '4' => Debug_Flag_4 := Val;
            when '5' => Debug_Flag_5 := Val;
            when '6' => Debug_Flag_6 := Val;
            when '7' => Debug_Flag_7 := Val;
            when '8' => Debug_Flag_8 := Val;
            when '9' => Debug_Flag_9 := Val;
         end case;
      end if;

      if C in LLet then
         case LLet'(C) is
            when 'a' => Debug_Flag_A := Val;
            when 'b' => Debug_Flag_B := Val;
            when 'c' => Debug_Flag_C := Val;
            when 'd' => Debug_Flag_D := Val;
            when 'e' => Debug_Flag_E := Val;
            when 'f' => Debug_Flag_F := Val;
            when 'g' => Debug_Flag_G := Val;
            when 'h' => Debug_Flag_H := Val;
            when 'i' => Debug_Flag_I := Val;
            when 'j' => Debug_Flag_J := Val;
            when 'k' => Debug_Flag_K := Val;
            when 'l' => Debug_Flag_L := Val;
            when 'm' => Debug_Flag_M := Val;
            when 'n' => Debug_Flag_N := Val;
            when 'o' => Debug_Flag_O := Val;
            when 'p' => Debug_Flag_P := Val;
            when 'q' => Debug_Flag_Q := Val;
            when 'r' => Debug_Flag_R := Val;
            when 's' => Debug_Flag_S := Val;
            when 't' => Debug_Flag_T := Val;
            when 'u' => Debug_Flag_U := Val;
            when 'v' => Debug_Flag_V := Val;
            when 'w' => Debug_Flag_W := Val;
            when 'x' => Debug_Flag_X := Val;
            when 'y' => Debug_Flag_Y := Val;
            when 'z' => Debug_Flag_Z := Val;
         end case;
      end if;
   end Set_Debug_Flag;

   -----------------------
   -- Set_Debug_Options --
   -----------------------

   procedure Set_Debug_Options (Options : String) is
      pragma Assert (Options /= "");
   begin
      for J in Options'Range loop
         Set_Debug_Flag (Options (J));
      end loop;
   end Set_Debug_Options;

end Utils_Debug;

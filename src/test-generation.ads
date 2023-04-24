------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                         Copyright (C) 2023, AdaCore                      --
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
--
--  Package defining the processing of units in order to generate test vectors
--  for the subprograms in a given unit.
--
--  These test vectors are to be inserted in the test harness, see
--  Test.Skeleton for this.

with Libadalang.Analysis; use Libadalang.Analysis;

with Utils.Command_Lines; use Utils.Command_Lines;

package Test.Generation is

   procedure Run_First_Pass_Tool (Cmd : Command_Line);
   --  Run a new instance of Test.Actions.Test_Tool that will process all
   --  sources to find the subprogram of interest and generate, build and run
   --  the generation harness.

   procedure Process_Source (Unit : Analysis_Unit);
   --  Iterate over the subprograms defined in Unit and include them in the
   --  TGen.Libgen context so that the subprograms get included in the value
   --  generation harness.

   procedure Generate_Build_And_Run (Cmd : Command_Line);
   --  Generate, build and run the test vector generation harness. This also
   --  run a second pass of the gnattest tool, to actually create a user facing
   --  test harness, unpacking the serialized tests just created.

end Test.Generation;

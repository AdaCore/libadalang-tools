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

with Libadalang.Analysis; use Libadalang.Analysis;
package Test.Skeleton is
   procedure Process_Source (The_Unit : Analysis_Unit);

   procedure Generate_Project_File (Source_Prj : String);
   --  Generates a project file that sets the value of Source_Dirs
   --  with the directories whe generated tests are placed and includes
   --  the argument project file.

   procedure Report_Unused_Generic_Tests;
   --  Outputs a warning message for generic UUTs that have been processed and
   --  have corresponding generic test packages generated, but no corresponding
   --  instantiations have been processed and thus tests for those UUTs are not
   --  included in the final harness.

   procedure Report_Tests_Total;
   --  Outputs number of processed subprograms in each of analyzed sources,
   --  number of new skeletons created and total number of tests.

end Test.Skeleton;

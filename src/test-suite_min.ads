------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2014-2024, AdaCore                    --
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

--  This package provides various utilities to minimize a testsuite in JSON
--  format.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Test.Common;         use Test.Common;
with Utils.Command_Lines; use Utils.Command_Lines;

package Test.Suite_Min is

   procedure Minimize_Suite (Cmd : Command_Line)
   with Pre => Test.Common.Harness_Has_Gen_Tests;
   --  Instrument, build and run the gnattest harness, in order to produce
   --  coverage traces for all the subprograms which have at least one
   --  generated test.
   --
   --  The minimize the testsuite in the gnattest harness. This only removes
   --  tests that are encoded as JSON values in the Test.Common.JSON_Test_Dir,
   --  but uses the user written tests as a baseline for coverage.
   --
   --  Use the support lib generation status as a proxy to indicate whether the
   --  test harness is generated and contains JSON tests.

end Test.Suite_Min;

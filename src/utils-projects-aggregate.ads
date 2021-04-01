------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  This package contains high-level interfaces to project files needed to
--  process aggregate projects.
--
--  There are two different cases here:
--
--  * Simple case  - aggregate project aggregates only one (non-aggregate)
--                   project that has sources. This case from the point of a
--                   tool does not differ from processing of non-aggregate
--                   project, except that when processing a project file, a
--                   tool should unload an aggregate project and load (the
--                   only) project with sources
--
--  * Complex case - aggregate project aggregates more than one project that
--                   has (or, more precisely, may have) sources. Processing of
--                   this case is described in the body of this package.
--
--  For the 'Complex case' when the tool is spawned from itself to process the
--  next aggregated project, the option '--aggregated-project-file prj' is
--  added to the command-line arguments. 'prj' is the name of an aggregated
--  project to be processed by this run. For both simple and complex cases the
--  processing is similar. First the tool loads an aggregate project -- this
--  sets values of all the project environment variables, Then it unloads the
--  aggregate project and loads a specific project (the parameter of the
--  --aggregated-project-file option in complex case) for actual processing.

package Utils.Projects.Aggregate is

   procedure Collect_Aggregated_Projects (P : Project_Type);
   --  Stores (in internal data structures) the full paths to the
   --  (non-aggregate!) projects that have been aggregated by P

   function Num_Of_Aggregated_Projects return Natural;
   --  Returns the number of (non-aggregate) projects being aggregated

   function Use_Subprocesses_For_Aggregated_Projects return Boolean is
     (Num_Of_Aggregated_Projects > 1);
   --  True if we should spawn a subprocess for each aggregated project
   --  belonging to the aggregate project. The case when an aggregate project
   --  aggregates only one project is treated specially. That is, False for the
   --  "Simple case", and True for the "Complex case".

   function Get_Aggregated_Prj_Src return String_Access with
      Pre => Num_Of_Aggregated_Projects = 1;
   --  Returns the (single!) aggregate project source file. This is called when
   --  we are not going to spawn any subprocesses.

   procedure Process_Aggregated_Projects
     (Cmd : Command_Line; Tool_Package_Name : String) with
       Pre => Num_Of_Aggregated_Projects > 1;
   --  Iterates through the projects being aggregated and spawns the tool
   --  for each of them.

end Utils.Projects.Aggregate;

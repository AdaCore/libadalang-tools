------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Libadalang.Analysis;
with Libadalang.Helpers;

procedure TGen_App is
   package Helpers renames Libadalang.Helpers;
   package LAL renames Libadalang.Analysis;

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);

   package App is new Helpers.App
     (Name         => "Libgen",
      Description  => "Type generation value for Ada",
      Process_Unit => Process_Unit);

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
   begin
      null;
   end Process_Unit;
begin
   App.Run;
end TGen_App;

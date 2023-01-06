------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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
--  Lint tools driver

with Ada.Text_IO;

with GNATCOLL.Traces;

with Lint.Command_Line;
with Lint.Tools;
with Lint.Tools.Array_Aggregates_Tool;

procedure Lint.Main is
begin
   if Lint.Command_Line.Parser.Parse then
      if Lint.Command_Line.Help.Get then
         Ada.Text_IO.Put_Line (Lint.Command_Line.Parser.Help);

      else
         if Lint.Command_Line.Verbose.Get then
            Lint.Logger.Set_Active (True);
         end if;

         case Lint.Command_Line.Tool.Get is
            when Lint.Tools.Array_Aggregates =>
               Lint.Tools.Array_Aggregates_Tool.Run;
         end case;
      end if;
   end if;
end Lint.Main;

------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2022-2024, AdaCore                      --
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

with GNATCOLL.Traces;

package TGen.Logging is

   subtype GNATCOLL_Trace is GNATCOLL.Traces.Logger;
   --  Convenience subtype so that trace creation requires "with TGen.Logging;"
   --  only instead of also "with GNATCOLL.Traces;".

   GNATCOLL_Trace_Prefix : constant String := "tgen.";
   --  Prefix to use for all GNATCOLL traces defined in TGen

   function Create_Trace (Unit_Name : String) return GNATCOLL_Trace;
   --  Wrapper around GNATCOLL.Traces.Create to create TGen-specific traces
   --  (with GNATCOLL_Trace_Prefix and standard settings).

end TGen.Logging;

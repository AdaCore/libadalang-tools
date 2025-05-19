------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2022-2025, AdaCore                      --
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

--  TGen trace module, this module is a very simple trace implementation that
--  is suitable for platforms that are not supported by GNATCOLL. Set the
--  Set the `TGEN_RTS_TRACE` environnment variable to enable traces. By default
--  no traces are emitted.
--
--  Usage:
--
--  with TGen.Logging; use TGen.Logging;
--  Me : constant TGen_Trace := Create_Trace ("My_Module");
--  ...
--  Trace (Me, +"Useful debug message");

with Ada.Strings.Unbounded;
with Ada.Text_IO;

package TGen.Logging is

   use type Ada.Text_IO.File_Access;

   type TGen_Trace is record
      Unit_Name : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
      Output    : Ada.Text_IO.File_Access := null;
   end record;

   TGen_Trace_Prefix : constant String := "tgen.";
   --  Prefix to use for all TGen traces.

   function Create_Trace
     (Unit_Name : Ada.Strings.Unbounded.Unbounded_String;
      Output    : Ada.Text_IO.File_Access := Ada.Text_IO.Standard_Error)
      return TGen_Trace;
   --  Create a TGen trace object for a given unit name. `Output` parameter
   --  can be used to control where logs are written. Logs are written to
   --  standard error (stderr) by default.

   procedure Trace (Self : TGen_Trace; Message : String)
   with Pre => Self.Output /= null;
   --  Write a message to the output with the configured package name.

end TGen.Logging;

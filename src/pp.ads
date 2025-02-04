------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2023, AdaCore                      --
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
--  Root of pretty-printing utilities

pragma Warnings (Off); -- imported for children
with Ada; use Ada;

with Ada.Exceptions;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;

with GNATCOLL.Traces;

with Utils;                  use Utils;
with Utils.Dbg_Out;
with Utils_Debug;            use Utils_Debug;
with Utils.String_Utilities; use Utils.String_Utilities;
pragma Warnings (On);

package Pp is

   type Log_Level is (Debug, Info, Error);

   procedure Trace (Message : String; Level : Log_Level := Info);
   --  Output Message to the stream associated with Level.
   --  If Level = Error and GNATpp is being used in bin mode, then also
   --  logs to stderr using Utils.Err_Out.

   procedure Trace
     (E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "Unexpected exception: ";
      Level   : Log_Level := Error);
   --  Output E information to the stream associated with Level.
   --  If Level = Error and GNATpp is being used in bin mode, then also
   --  logs to stderr using Utils.Err_Out.

private

   Info_Logger  : constant GNATCOLL.Traces.Logger :=
     GNATCOLL.Traces.Create ("GNATPP.INFO");
   Debug_Logger : constant GNATCOLL.Traces.Logger :=
     GNATCOLL.Traces.Create ("GNATPP.DEBUG");
   Error_Logger : constant GNATCOLL.Traces.Logger :=
     GNATCOLL.Traces.Create ("GNATPP.ERROR");

end Pp;

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

with Langkit_Support.Slocs; use Langkit_Support;
with Pp.Scanner;
package Pp.Error_Slocs is

   Error_Sloc : Slocs.Source_Location := Slocs.No_Source_Location;
   --  Global variable (!) that stores the current source location being
   --  processed by gnatpp. This is used in case gnatpp crashes; we print an
   --  error message containing the source location if possible, to give the
   --  user a hint as to which construct in their code caused the crash.
   --  This is set in the various passes. It is not guaranteed to be accurate.

   function To_Langkit
     (Sloc : Scanner.Source_Location) return Slocs.Source_Location is
      ((Slocs.Line_Number (Sloc.Line), Slocs.Column_Number (Sloc.Col)));
   --  Convert Scanner.Source_Location to Langkit's version of
   --  Source_Location.

end Pp.Error_Slocs;

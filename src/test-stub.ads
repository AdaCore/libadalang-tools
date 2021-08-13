------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2014-2021, AdaCore                    --
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

--  This package defines different routines for generating stub files.

with Libadalang.Analysis;

package Test.Stub is

   procedure Process_Unit
     (Pack                : Libadalang.Analysis.Ada_Node;
      Body_File_Name      : String;
      Stub_Data_File_Spec : String;
      Stub_Data_File_Body : String);
   --  Processes corresponding spec and body,
   --  (re)creates stub body and stub data package.

   Stub_Processing_Error : exception;
   --  Indicates that an unhandled error occured during the processing of given
   --  unit and stub has not being generated partially or completely and thus
   --  is unusable.

end Test.Stub;

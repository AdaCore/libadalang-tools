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

package Utils.Strings is

   function Has_Prefix (X, Prefix : String) return Boolean;
   --  True if Prefix is at the beginning of X. For example,
   --  Has_Prefix("An_Identifier", Prefix => "An_") is True.

   function Has_Suffix (X, Suffix : String) return Boolean;
   --  True if Suffix is at the end of X

   function Replace_String (S, From, To : String) return String;
   --  Replaces all occurrences of From in S with To

   function Dashes
     (S : String) return String is
     (Replace_String (S, From => "_", To => "-"));

end Utils.Strings;

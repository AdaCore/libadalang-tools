------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--
--  Provides translation utilities for types represented by Libadalang nodes
--  to TGen's internal type representation.

package TGen.Types.Translation is

   type Translation_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Res : SP.Ref;
         when False =>
            Diagnostics : Unbounded_String :=
              To_Unbounded_String ("Error during translation");
      end case;
   end record;

   function Translate
     (N       : LAL.Type_Expr;
      Verbose : Boolean := False) return Translation_Result;
   --  Translate N to TGen's internal type representation

   function Translate
     (N : LAL.Base_Type_Decl;
      Verbose : Boolean := False) return Translation_Result;
   --  Translate N to TGen's internal type representation

   procedure Print_Cache_Stats;
   --  Print translation cache statistics on the standard output

   procedure PP_Cache;
   --  Print the content of the translation cache on the standard output

   procedure Clear_Cache;
   --  Clear the translation cache

end TGen.Types.Translation;

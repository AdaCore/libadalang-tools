------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Text_IO; use Ada.Text_IO;

package TGen.Marshalling.JSON_Marshallers is

   procedure Generate_Marshalling_Functions_For_Typ
     (F_Spec, F_Body     : File_Type;
      Typ                : TGen.Types.Typ'Class;
      Templates_Root_Dir : String);
   --  Generate JSON marshalling and unmarshalling functions for Typ. Note that
   --  this function will not operate recursively. It will thus have to be
   --  called for each of the component type of a record for instance.
   --
   --  We generate the following functions:
   --
   --  function TAGAda_Marshalling_Typ_Output
   --    (TAGAda_Marshalling_V : Typ) return TGen.JSON.JSON_Value;
   --
   --  function TAGAda_Marshalling_Typ_Input
   --    (TAGAda_Marshalling_JSON : TGen.JSON.JSON_Value)
   --    return Typ;
   --
   --  Templates_Root_Dir should be the path to the root directory in which all
   --  TGen templates are stored.
   --
   --  TODO: right now, this also needs the binary marshallers to have been
   --  generated before, as we need the header type (for unconstrained type)
   --  that they generate. This should be splitted from the generation of
   --  binary marshallers.

end TGen.Marshalling.JSON_Marshallers;

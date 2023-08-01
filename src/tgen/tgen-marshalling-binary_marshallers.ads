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

package TGen.Marshalling.Binary_Marshallers is

   procedure Generate_Marshalling_Functions_For_Typ
     (F_Spec, F_Body     : File_Type;
      Typ                : TGen.Types.Typ'Class;
      Part               : Spec_Part;
      Templates_Root_Dir : String);
   --  Generate binary marshalling and unmarshalling functions for Typ. Note
   --  that this function will not operate recursively. It will thus have to
   --  be called for each of the component type of a record for instance.
   --
   --  Part determines which part (public or private) of the spec will be
   --  generated. It is thus necessary to call this subprogram twice in order
   --  to generate a full spec, taking care to insert a "private" line in
   --  F_Spec in between the two calls. The body is generated at the same time
   --  the public part is generated, nothing will be written to F_Body if Part
   --  is Priv.
   --
   --  If the type does not need a header, we generate:
   --
   --  procedure TAGAda_Marshalling_Typ_Output
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  function TAGAda_Marshalling_Typ_Input
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class)
   --    return Typ;
   --
   --  --  Otherwise, we generate:
   --
   --  type TAGAda_Marshalling_Typ_Header_Type is record
   --     < Typ's array bound or record discriminants >
   --  end record;
   --
   --  function TAGAda_Marshalling_Typ_Input_Header
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class)
   --    return TAGAda_Marshalling_Typ_Header_Type;
   --
   --  procedure TAGAda_Marshalling_Typ_Output_Header
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  function TAGAda_Marshalling_Typ_Size_Header return Natural;
   --
   --  procedure TAGAda_Marshalling_Typ_Output
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_V      : Shape);
   --
   --  function TAGAda_Marshalling_Typ_Input
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_H      : TAGAda_Marshalling_Typ_Header_Type)
   --    return Typ;
   --
   --  Templates_Root_Dir should be the path to the root directory in which all
   --  TGen templates are stored.

end TGen.Marshalling.Binary_Marshallers;

------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2021-2023, AdaCore                    --
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
--  This unit provides a procedure to generate a marshalling/un-marshalling
--  function for a given Ada type.

with Ada.Text_IO; use Ada.Text_IO;
with TGen.Types;  use TGen.Types;

package TGen.Marshalling is

   function Is_Supported_Type (Typ : TGen.Types.Typ'Class) return Boolean;
   --  Return True for types which are currently supported by the prototype

   procedure Generate_Marshalling_Functions_For_Typ
     (F_Spec, F_Body     : File_Type;
      Typ                : TGen.Types.Typ'Class;
      Templates_Root_Dir : String)
   with Pre => Is_Supported_Type (Typ);
   --  Generate marshalling and unmarshalling functions for Typ.
   --
   --  --  We generate:
   --
   --    function TAGAda_Marshalling_Typ_Size_Header return Natural;
   --  or
   --    TAGAda_Marshalling_Typ_Size_Header : constant := ...;
   --
   --  procedure TAGAda_Marshalling_Typ_Output
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  function TAGAda_Marshalling_Typ_Input
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class)
   --    return Typ;
   --
   --  procedure TAGAda_Marshalling_Typ_Output
   --    (TAGAda_Marshalling_Header : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  function TAGAda_Marshalling_Typ_Input
   --    (TAGAda_Marshalling_Header : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class)
   --    return Typ;
   --
   --  Templates_Root_Dir should be the path to the root directory in which all
   --  TGen templates are stored.

end TGen.Marshalling;

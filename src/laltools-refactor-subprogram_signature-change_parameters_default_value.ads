------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  This package contains the Change Parameters Type refactoring tool that
--  allow changing the default value of one or more parameters of a subprogram.

package Laltools.Refactor.Subprogram_Signature.
          Change_Parameters_Default_Value
is

   function Is_Change_Parameters_Default_Value_Available
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range)
      return Boolean;
   --  Checks if Parameters_Source_Location_Range correspondes to a selection
   --  of one or more parameters of the same Param_Spec. If so, returns True
   --  and sets New_Parameter_Syntax_Rules to the allowed syntax rules for the
   --  new parameter. Otherwise returns False and sets
   --  New_Parameter_Syntax_Rules to an empty vector.

   type Parameters_Default_Value_Changer is new
     Subprogram_Signature_Changer with private;

   function Create_Parameters_Default_Value_Changer
     (Unit                             : Analysis_Unit;
      Parameters_Source_Location_Range : Source_Location_Range;
      New_Parameters_Default_Value     : Unbounded_String;
      Configuration                    :
        Signature_Changer_Configuration_Type := Default_Configuration)
      return Parameters_Default_Value_Changer;
   --  Parameters_Type_Changer constructor.
   --  Creates a subprogram signature changer that changes parameters type.
   --  Parameters_Source_Location_Range must be the Source_Location_Range
   --  of the parameters, where Start_Sloc (Parameters_Source_Location_Range)
   --  is the first parameter Name node, and
   --  End_Sloc (Parameters_Source_Location_Range) is the last one. If they're
   --  the same, then only one parameter is modified.
   --  Configuration controls what subprograms in the hierarchy are affected.
   --  Unit and Parameters_Source_Location_Range should be validated by
   --  Is_Change_Parameters_Type_Available.

   overriding
   function Refactor
     (Self           : Parameters_Default_Value_Changer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Returns an Edit_Map with all the refactoring edits needed to change
   --  the parameters type.

private

   type Parameters_Default_Value_Changer is new
     Subprogram_Signature_Changer with
      record
         Unit                             : Analysis_Unit;
         Parameters_Source_Location_Range : Source_Location_Range;
         New_Parameters_Default_Value     : Unbounded_String;
         Configuration                    :
           Signature_Changer_Configuration_Type := Default_Configuration;
      end record;
end Laltools.Refactor.Subprogram_Signature.Change_Parameters_Default_Value;

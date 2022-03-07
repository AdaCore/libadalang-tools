------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2012-2022, AdaCore                    --
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

generic
   type Char_Type is (<>); -- e.g. Character or Wide_Character
   type Str_Type is array (Positive range <>) of Char_Type;
   --  e.g. String or Wide_String
   with procedure Basic_Put_Char (C : Char_Type);
   Output_Enabled_Initially : Boolean := True;
package Utils.Generic_Formatted_Output is

   --  Simple formatted output.

   --  Client passes in Basic_Put_Char, which determines where the output
   --  characters go.

   type Template is new String;
   procedure Put
     (T                      : Template;
      X1, X2, X3, X4, X5, X6 : Str_Type := []);
   --  Prints the template as is, except for the following escape characters:
   --    "\n" is end of line.
   --    "\r" is ASCII.CR (carriage return).
   --    "\t" is ASCII.HT (tab character).
   --    "\i" indents, and "\o" outdents.
   --    "\1" is replaced with X1, and similarly for 2, 3, ....
   --    "\\" is "\".

   procedure Put_Char (C : Char_Type);
   --  Same as Put ("\1", (1 => C));

   procedure Put_Str (S : Str_Type);
   --  Same as Put ("\1", S);

   Default_Indentation_Amount : constant Natural := 3;

   procedure Indent
     (Indentation_Amount : Natural := Default_Indentation_Amount);
   procedure Outdent
     (Indentation_Amount : Natural := Default_Indentation_Amount);
   --  Increase/decrease indentation level by given number of spaces

   function Cur_Column return Positive;
   --  Current output column. The next output character will be in this column.

   procedure Tab_To_Column (Column : Positive);
   --  Put spaces until we're at or past Column.

   Output_Enabled : Boolean := Output_Enabled_Initially;
   --  Set this to False to send all output into the bit bucket. Used for
   --  debugging output.

   Indentation_Level : Natural := 0;
   --  Current indentation level. We deliberately expose this variable
   --  to clients, because some need to directly get/set this, bypassing
   --  In/Outdent.

end Utils.Generic_Formatted_Output;

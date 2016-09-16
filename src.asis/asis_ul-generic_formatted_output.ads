------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--              G E N E R I C _ F O R M A T T E D _ O U T P U T             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

generic
   type Char_Type is (<>); -- e.g. Character or Wide_Character
   type Str_Type is array (Positive range <>) of Char_Type;
   --  e.g. String or Wide_String
   with procedure Basic_Put_Char (C : Char_Type);
   Output_Enabled_Initially : Boolean := True;
package ASIS_UL.Generic_Formatted_Output is

   --  Simple formatted output.

   --  Client passes in Basic_Put_Char, which determines where the output
   --  characters go.

   type Template is new String;
   procedure Put
     (T                      : Template;
      X1, X2, X3, X4, X5, X6 : Str_Type := (1 .. 0 => <>));
   --  Prints the template as is, except for the following escape characters:
   --    "\n" is end of line.
   --    "\t" is ASCII.HT (tab character).
   --    "\i" indents, and "\o" outdents.
   --    "\1" is replaced with X1, and similarly for 2, 3, ....
   --    "\\" is "\".

   procedure Put_Char (C : Char_Type);
   --  Same as Put ("\1", (1 => C));

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

   Indentation : Natural := 0;
   --  Current indentation level. We deliberately expose this variable
   --  to clients, because some need to directly get/set this, bypassing
   --  In/Outdent.

end ASIS_UL.Generic_Formatted_Output;

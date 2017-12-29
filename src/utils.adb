------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                              A S I S _ U L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2017, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not, write to the Free Software Foundation, 59 Temple Place  --
--  - Suite 330, Boston,                                                    --
--                                                                          --
-- ASIS UL is maintained by ACT Europe (http://www.act-europe.fr).          --

package body Utils is

   procedure Set_Assert_Enabled;
   --  This is to get around the syntactic restrictions of pragma Debug, which
   --  takes a procedure call as parameter (pretending that's an expression).
   --  The call below will not happen in assertions-off mode, thus leaving
   --  Assert_Enabled = False.

   procedure Set_Assert_Enabled is
   begin
      Assert_Enabled := True;
   end Set_Assert_Enabled;

begin
   pragma Debug (Set_Assert_Enabled);
end Utils;

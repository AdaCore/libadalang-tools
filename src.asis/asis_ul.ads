------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                              A S I S _ U L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2004-2007, AdaCore                      --
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
--                                                                          --
------------------------------------------------------------------------------

--  The top of Asis Utility Library (ASIS UL) hierarchy
--
--  ASIS UL originated from the idea to factor out the common components of the
--  functionality from the ASIS-based tools developed at ACT/ACT Europe. It
--  accumulates the "driver-level" functionality, for the useful ASIS secondary
--  queries and ASIS extensions see Asis.Extensions hierarchy in the asis/asis
--  directory. ASIS UL contains such things as diagnostic message reporting,
--  detecting the compiler to be used to create the tree for the ASIS part of
--  a tool, dealing with temporary directory where tool storing these trees
--  etc.
--
--  ASIS UL may be used by GNAT Pro ASIS users to create ASIS tools that follow
--  some general design rules (to be described in full details later...)

package ASIS_UL is

   Main_Done : Boolean := False;
   --  This is set True at the (successful) end of each main procedure. The
   --  purpose is so assertions in Finalize operations can tell whether the
   --  main procedure exited normally. See, for example,
   --  Generic_Formatted_Output.Finalize, which insists that when we reach the
   --  end of the main procedure, the indentation level should be zero. But if
   --  an exception propagates out of the main procedure, that's just a bug
   --  which should be reported normally.

end ASIS_UL;

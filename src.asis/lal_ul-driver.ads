------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . D R I V E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2013-2014, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This is the version of the top-level tool driver that implements a tool
--  that has one or more source file arguments. It does make use of the source
--  file table. It allows to have a direct project support in a tool.

with ASIS_UL.Projects;
with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;

procedure LAL_UL.Driver
  (Prj                   : in out ASIS_UL.Projects.Arg_Project_Type'Class;
   Cmd                   : in out Command_Line;
   Tool_Package_Name     :        String;
   Needs_Per_File_Output :        Boolean        := False;
   No_Preprocessing      :        Boolean        := False;
   Callback              :        Parse_Callback := null);
--  No_Preprocessing needs to be ON for gnatpp, because it is updating the
--  source. It is also ON for gnat2xml; not sure why.

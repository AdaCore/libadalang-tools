------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                  G N A T P P . D I C T I O N A R I E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston,                                                                  --
--                                                                          --
-- GNATPP is maintained by ACT Europe (http://www.act-europe.fr).           --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines for dealing with the casing exception
--  dictionaries

package Pp.Formatting.Dictionaries is

   procedure Reset_Dictionary;
   --  Resets the tables to empty

   procedure Scan_Dictionary (Dictionary_Name : String);
   --  Scans the dictionary file whose name is Dictionary_Name and stores all
   --  the casing exceptions in the exception tables.

   procedure Check_With_Dictionary
     (Ada_Name : in out Wide_String;
      Casing   : PP_Casing);
   --  Checks if Ada_Name as a whole or some its subname (that is, a part of
   --  the Ada_Name surrounded by '_' is in the exception dictionary, and if it
   --  is, changes the casing of Ada_Name or of its part to what is defined in
   --  the dictionary. For the names or name parts that are not in the
   --  dictionary, changes their casing according to the value of Casing
   --  parameter

end Pp.Formatting.Dictionaries;

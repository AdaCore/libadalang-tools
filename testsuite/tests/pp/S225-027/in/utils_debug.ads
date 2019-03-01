------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                        A S I S _ U L . D E B U G                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2009-2017, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING.  If  not,  write  to  the Free Software Foundation, 51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains global flags used to control the generation of
--  debugging information in various reusable components used in ASIS-based
--  tools. Some of these flags can also used by tools themselves.

package Utils_Debug is

   --  The structure of this package is similar to the GNAT Debug package

   -------------------------
   -- Dynamic Debug Flags --
   -------------------------

   --  Flags that can be used to active various specialized debugging output
   --  information. The flags are preset to False, which corresponds to the
   --  given output being suppressed. The individual flags can be turned on
   --  using the undocumented switch dxxx where xxx is a string of letters for
   --  flags to be turned on. Documentation on the current usage of these flags
   --  is contained in the body of this package

   --  -d<low_case_letter> flags are supposed to be used as general flags
   --  applicable to all the tools based on Utils.

   Debug_Flag_A : Boolean := False;
   Debug_Flag_B : Boolean := False;
   Debug_Flag_C : Boolean := False;
   Debug_Flag_D : Boolean := False;
   Debug_Flag_E : Boolean := False;
   Debug_Flag_F : Boolean := False;
   Debug_Flag_G : Boolean := False;
   Debug_Flag_H : Boolean := False;
   Debug_Flag_I : Boolean := False;
   Debug_Flag_J : Boolean := False;
   Debug_Flag_K : Boolean := False;
   Debug_Flag_L : Boolean := False;
   Debug_Flag_M : Boolean := False;
   Debug_Flag_N : Boolean := False;
   Debug_Flag_O : Boolean := False;
   Debug_Flag_P : Boolean := False;
   Debug_Flag_Q : Boolean := False;
   Debug_Flag_R : Boolean := False;
   Debug_Flag_S : Boolean := False;
   Debug_Flag_T : Boolean := False;
   Debug_Flag_U : Boolean := False;
   Debug_Flag_V : Boolean := False;
   Debug_Flag_W : Boolean := False;
   Debug_Flag_X : Boolean := False;
   Debug_Flag_Y : Boolean := False;
   Debug_Flag_Z : Boolean := False;

   --  -d<digit> flags are supposed to use in tool-specific manner, so they
   --   should not be used in the code of Utils
   Debug_Flag_1 : Boolean := False;
   Debug_Flag_2 : Boolean := False;
   Debug_Flag_3 : Boolean := False;
   Debug_Flag_4 : Boolean := False;
   Debug_Flag_5 : Boolean := False;
   Debug_Flag_6 : Boolean := False;
   Debug_Flag_7 : Boolean := False;
   Debug_Flag_8 : Boolean := False;
   Debug_Flag_9 : Boolean := False;

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True);
   --  Where C is a-z or 0-9, sets the corresponding debug flag to
   --  the given value. Does nothing if C is not in a-z or 0-9.

   procedure Set_Debug_Options (Options : String);
--  Sets debug flags treating its argument as a parameter of '-d' option.

end Utils_Debug;

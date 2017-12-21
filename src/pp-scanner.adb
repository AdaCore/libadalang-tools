------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . S C A N N E R                     --
--                                                                          --
--                                 B o d y                                  --
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

package body Pp.Scanner is

   function Leading_Blanks (X : Token) return Natural is
   begin
      pragma Assert (X.Kind in Comment_Kind);
      return X.Leading_Blanks;
   end Leading_Blanks;

   function Width (X : Token) return Natural is
   begin
      pragma Assert (X.Kind in Whole_Line_Comment);
      return X.Width;
   end Width;

   function First_Pos
     (Input : Buffer;
      Sloc  : Source_Location)
      return  Positive
   is
   begin
      return Result : constant Positive := Position (Input, Sloc.Firstx) do
         pragma Assert (Result = Sloc.First);
      end return;
   end First_Pos;

   function Last_Pos (Input : Buffer; Sloc : Source_Location) return Natural is
   begin
      return Result : constant Natural := Position (Input, Sloc.Lastx) - 1 do
         pragma Assert (Result = Sloc.Last);
      end return;
   end Last_Pos;

   function Line_Length
     (Input    : in out Buffer;
      Ends     : Marker_Vector;
      Line_Num : Positive)
      return     Natural
   is

      M1 : constant Marker   := Ends (Marker_Index (Line_Num));
      P1 : constant Positive := Position (Input, M1);
      M0 : Marker;
      P0 : Natural;

   begin
      if Line_Num = 1 then
         P0 := 0;

      else
         M0 := Ends (Marker_Index (Line_Num - 1));
         P0 := Position (Input, M0);
      end if;
      return P1 - P0 - 1;
   end Line_Length;

end Pp.Scanner;

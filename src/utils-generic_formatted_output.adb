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

with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;

with Ada.Finalization; use Ada.Finalization;

with Utils.String_Utilities; use Utils.String_Utilities;

package body Utils.Generic_Formatted_Output is
   package Dummy is
   end Dummy;
   package body Dummy is
   begin
      --  Turn on symbolic tracebacks on unhandled exceptions. We put this here
      --  to make it happen as early as possible. It is supposed to be done in
      --  System.Traceback.Symbolic (s-trasym-dwarf.adb), but that's not
      --  working for some reason.

      GNAT.Exception_Traces.Set_Trace_Decorator
        (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
      GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);
   end Dummy;

   Column : Natural := 1;

   procedure Raw_Put_Char (C : Char_Type);
   --  Put the character and adjust Column

   type Finalization is new Limited_Controlled with null record;
   procedure Finalize (X : in out Finalization);
   The_Finalization : Finalization;
   pragma Unreferenced (The_Finalization);
   --  Declare a singleton object to check that the indentation isn't messed up
   --  -- we should end up at zero indentation.

   ----------------
   -- Cur_Column --
   ----------------

   function Cur_Column return Positive is
   begin
      return Column;
   end Cur_Column;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Finalization) is
      pragma Unreferenced (X);
   begin
      if Main_Done then
         if Indentation_Level /= 0 then
            raise Program_Error with "Indentation should be zero at end.";
         end if;
      end if;
   end Finalize;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (Indentation_Amount : Natural := Default_Indentation_Amount)
   is
   begin
      Indentation_Level := Indentation_Level + Indentation_Amount;
   end Indent;

   -------------
   -- Outdent --
   -------------

   procedure Outdent
     (Indentation_Amount : Natural := Default_Indentation_Amount)
   is
   begin
      Indentation_Level := Indentation_Level - Indentation_Amount;
   end Outdent;

   ---------
   -- Put --
   ---------

   procedure Put
     (T                      : Template;
      X1, X2, X3, X4, X5, X6 : Str_Type := [])
   is
      J    : Positive                  := T'First;
      Used : array (1 .. 6) of Boolean := [others => False];

   begin
      if not Output_Enabled then
         return;
      end if;

      while J <= T'Last loop
         if T (J) = '\' then
            J := J + 1;
            case T (J) is
               when 'n' =>
                  Put_Char (Char_Type'Val (W_Char'Pos (NL)));

               when 'r' =>
                  Put_Char (Char_Type'Val (W_Char'Pos (W_CR)));

               when 't' =>
                  Put_Char (Char_Type'Val (W_Char'Pos (W_HT)));

               when '\' =>
                  Put_Char (Char_Type'Val (W_Char'Pos ('\')));

               when 'i' =>
                  Indent;

               when 'o' =>
                  Outdent;

               when '1' =>
                  Used (1) := True;
                  Put_Str (X1);

               when '2' =>
                  Used (2) := True;
                  Put_Str (X2);

               when '3' =>
                  Used (3) := True;
                  Put_Str (X3);

               when '4' =>
                  Used (4) := True;
                  Put_Str (X4);

               when '5' =>
                  Used (5) := True;
                  Put_Str (X5);

               when '6' =>
                  Used (6) := True;
                  Put_Str (X6);

               when others =>
                  raise Program_Error;
            end case;

         else
            Put_Char (Char_Type'Val (Character'Pos (T (J))));
         end if;
         J := J + 1;
      end loop;

      if not Used (1) then
         pragma Assert (X1'Length = 0);
      end if;
      if not Used (2) then
         pragma Assert (X2'Length = 0);
      end if;
      if not Used (3) then
         pragma Assert (X3'Length = 0);
      end if;
      if not Used (4) then
         pragma Assert (X4'Length = 0);
      end if;
      if not Used (5) then
         pragma Assert (X5'Length = 0);
      end if;
      if not Used (6) then
         pragma Assert (X6'Length = 0);
      end if;
   end Put;

   procedure Put_Str (S : Str_Type) is
   begin
      for J in S'Range loop
         Put_Char (S (J));
      end loop;
   end Put_Str;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (C : Char_Type) is
   begin
      pragma Assert (Output_Enabled); -- too slow to check on every char
      if False and then not Output_Enabled then
         return;
      end if;

      if Column = 1 and then C /= Char_Type'Val (W_Char'Pos (NL)) then
         for J in 1 .. Indentation_Level mod 60 loop
            --  The "mod 60" is so we don't indent by huge amounts
            Raw_Put_Char (Char_Type'Val (W_Char'Pos (' ')));
         end loop;
      end if;
      Raw_Put_Char (C);
   end Put_Char;

   ------------------
   -- Raw_Put_Char --
   ------------------

   procedure Raw_Put_Char (C : Char_Type) is
   begin
      Basic_Put_Char (C);

      if C = Char_Type'Val (W_Char'Pos (NL)) then
         Column := 1;

      else
         Column := Column + 1;
      end if;
   end Raw_Put_Char;

   -------------------
   -- Tab_To_Column --
   -------------------

   procedure Tab_To_Column (Column : Positive) is
   begin
      while Cur_Column < Column loop
         Put_Char (Char_Type'Val (W_Char'Pos (' ')));
      end loop;
   end Tab_To_Column;

end Utils.Generic_Formatted_Output;

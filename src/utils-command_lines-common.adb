------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with GNATCOLL.Iconv;
package body Utils.Command_Lines.Common is

   procedure Set_WCEM (Cmd : in out Command_Line; Encoding : String) is
   begin
      if not Present (Arg (Cmd, Wide_Character_Encoding)) then
         Set_Arg (Cmd, Wide_Character_Encoding, Encoding);
      elsif Arg (Cmd, Wide_Character_Encoding).all /= Encoding then
         Cmd_Error_No_Help
           ("input and output wide character encodings conflict");
      end if;
   end Set_WCEM;

   function WCEM (Cmd : Command_Line) return Character;
   --  Return the single-character encoding letter

   function WCEM (Cmd : Command_Line) return Character is
      WCEM : constant String :=
        (if Arg (Cmd, Wide_Character_Encoding) = null
           then "b"
           else Arg (Cmd, Wide_Character_Encoding).all);
      C : constant Character := WCEM (1);
   begin
      --  We actually only support -Ws, -W8, and -Wb.

      if WCEM'Length /= 1 or else C not in 's' | '8' | 'b' then
         Cmd_Error ("unrecognized wide character encoding: """ & WCEM & """");
      end if;

      return C;
   end WCEM;

   function Wide_Character_Encoding (Cmd : Command_Line) return String is
   begin
      return
        (case WCEM (Cmd) is
           when 'h'    => "Hex", -- not supported by GNATCOLL.Iconv
           when 'u'    => "Upper", -- not supported by GNATCOLL.Iconv
           when 's'    => GNATCOLL.Iconv.SHIFT_JIS,
           when 'E'    => "EUC", -- not supported by GNATCOLL.Iconv
           when '8'    => GNATCOLL.Iconv.UTF8,
           when 'b'    => GNATCOLL.Iconv.ISO_8859_1, -- BRACKETS
           when others => raise Program_Error);
   end Wide_Character_Encoding;

   function Wide_Character_Encoding
     (Cmd : Command_Line) return System.WCh_Con.WC_Encoding_Method
   is
      use System.WCh_Con;
   begin
      return
        (case WCEM (Cmd) is
           when 'h'    => WCEM_Hex,
           when 'u'    => WCEM_Upper,
           when 's'    => WCEM_Shift_JIS,
           when 'e'    => WCEM_EUC,
           when '8'    => WCEM_UTF8,
           when 'b'    => WCEM_Brackets,
           when others => raise Program_Error);
   end Wide_Character_Encoding;

end Utils.Command_Lines.Common;

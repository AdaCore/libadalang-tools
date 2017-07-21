with GNATCOLL.Iconv;
package body Utils.Command_Lines.Common is

   function Wide_Character_Encoding (Cmd : Command_Line) return String is
      --  ???Libadalang doesn't support all the encodings we need.

      WCEM : constant String := Arg (Cmd, Wide_Character_Encoding).all;
      pragma Assert (WCEM'Length = 1);
      C : constant Character := WCEM (1);
   begin
      return
        (case C is
           when 'h'    => "Hex",
           when 'u'    => "Upper",
           when 's'    => "Shift_JIS",
           when 'E'    => "EUC",
           when '8'    => GNATCOLL.Iconv.UTF8,
           when 'b'    => GNATCOLL.Iconv.ISO_8859_1, -- BRACKETS
           when others => raise Program_Error);
   end Wide_Character_Encoding;

   function Wide_Character_Encoding
     (Cmd : Command_Line) return System.WCh_Con.WC_Encoding_Method
   is
      WCEM : constant String := Arg (Cmd, Wide_Character_Encoding).all;
      pragma Assert (WCEM'Length = 1);
      C : constant Character := WCEM (1);
      use System.WCh_Con;
   begin
      return
        (case C is
           when 'h'    => WCEM_Hex,
           when 'u'    => WCEM_Upper,
           when 's'    => WCEM_Shift_JIS,
           when 'e'    => WCEM_EUC,
           when '8'    => WCEM_UTF8,
           when 'b'    => WCEM_Brackets,
           when others => raise Program_Error);
   end Wide_Character_Encoding;

end Utils.Command_Lines.Common;

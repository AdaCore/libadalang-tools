with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Utils.Tool_Names is

   -------------------
   -- Detect_Target --
   -------------------

   function Target return String is
      Tgt_Last : constant Natural := Index (Tool_Name, "-", Backward);
      AAMP_Idx : constant Natural := Index (Tool_Name, "gnaamp");
   begin
      if AAMP_Idx = Tool_Name'First then
         return "AAMP";
      elsif Tgt_Last > 0 then
         return Tool_Name (Tool_Name'First .. Tgt_Last - 1);
      else
         return "";
      end if;
   end Target;

end Utils.Tool_Names;

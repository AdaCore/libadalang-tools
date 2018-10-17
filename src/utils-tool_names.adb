with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Utils.Tool_Names is

   ------------
   -- Target --
   ------------

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

   ---------------------
   -- Basic_Tool_Name --
   ---------------------

   function Basic_Tool_Name return String is
      Tgt_Last : constant Natural := Index (Tool_Name, "-", Backward);
   begin
      if Tgt_Last > 0 then
         return Tool_Name (Tgt_Last + 1 .. Tool_Name'Last);
      else
         return Tool_Name;
      end if;
   end Basic_Tool_Name;

end Utils.Tool_Names;

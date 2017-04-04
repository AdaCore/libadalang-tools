--=======================================================================--

package body F650a00.S is

   procedure Handle (Sa : in out Special_Alert) is
   begin
      F650a00.P.Practice_Alert (Sa).Handle;
      Sa.Display := Big_Screen;
   end Handle;

   function Make_Alert_For_Time (Time : in Duration) return Special_Alert is
   begin
      return Result : Special_Alert (Age => 39) do
         Set_Alert_Time (Result, Time);
      end return;
   end Make_Alert_For_Time;

end F650a00.S;

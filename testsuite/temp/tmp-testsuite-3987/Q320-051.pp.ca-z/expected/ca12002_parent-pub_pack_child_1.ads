private with Ca12002_Parent.Priv_Pack_Child;
private with Ca12002_Parent.Priv_Proc_Child;
private with Ca12002_Parent.Priv_Func_Child;
pragma Elaborate (Ca12002_Parent.Priv_Pack_Child);
pragma Elaborate_All (Ca12002_Parent.Priv_Func_Child);
package Ca12002_Parent.Pub_Pack_Child_1 is
   X : constant Integer;
   Y : constant Boolean;
   procedure P (X : out Integer);
   function F return Integer;
   package Nested is
      Z : constant Integer;
   private
      Z : constant Integer := Ca12002_Parent.Priv_Pack_Child.X * 3;
   end Nested;
private
   X : constant Integer := Ca12002_Parent.Priv_Pack_Child.X;
   Y : constant Boolean := Ca12002_Parent.Priv_Func_Child;
end Ca12002_Parent.Pub_Pack_Child_1;

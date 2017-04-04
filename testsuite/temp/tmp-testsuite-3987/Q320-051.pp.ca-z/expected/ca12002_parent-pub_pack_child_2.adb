with Ca12002_Parent.Pub_Pack_Child_2.Priv_Grandchild.Pub_Greatgrandchild;
package body Ca12002_Parent.Pub_Pack_Child_2 is

   function F return Float is
   begin
      return Ca12002_Parent.Pub_Pack_Child_2.Priv_Grandchild
          .Pub_Greatgrandchild
          .X;
   end F;

end Ca12002_Parent.Pub_Pack_Child_2;

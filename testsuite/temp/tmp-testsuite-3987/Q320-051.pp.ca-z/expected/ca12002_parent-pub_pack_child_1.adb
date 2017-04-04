package body Ca12002_Parent.Pub_Pack_Child_1 is
   procedure P (X : out Integer) renames Ca12002_Parent.Priv_Proc_Child;
   function F return Integer is separate;
end Ca12002_Parent.Pub_Pack_Child_1;

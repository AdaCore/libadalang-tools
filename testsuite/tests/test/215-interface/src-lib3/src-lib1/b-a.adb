with C;
pragma Unreferenced (C);

package body B.A is
   function BA_Func return Boolean is
   begin
      return Dummy_BA;
   end BA_Func;
end B.A;

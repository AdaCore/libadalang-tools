with Report; use Report;
package body C83030c_Decl1 is
   procedure C83030c_Proc1 is
   begin
      Global := Ident_Int (1);
   end C83030c_Proc1;

   procedure C83030c_Proc1 (X : Integer) is
   begin
      Global := Ident_Int (X);
   end C83030c_Proc1;

   procedure C83030c_Proc2 is
   begin
      Global := Ident_Int (1);
   end C83030c_Proc2;

   procedure C83030c_Proc2 (X : Integer) is
   begin
      Global := Ident_Int (X);
   end C83030c_Proc2;

   function C83030c_Func3 return Integer is
   begin
      return Ident_Int (1);
   end C83030c_Func3;

   function C83030c_Func3 return Boolean is
   begin
      return Ident_Bool (False);
   end C83030c_Func3;

   function C83030c_Func3 (X : Integer) return Integer is
   begin
      return Ident_Int (X);
   end C83030c_Func3;

   function C83030c_Func4 return Integer is
   begin
      return Ident_Int (1);
   end C83030c_Func4;

   function C83030c_Func4 return Boolean is
   begin
      return Ident_Bool (False);
   end C83030c_Func4;
end C83030c_Decl1;

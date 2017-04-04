with Report;        use Report;
with C83030c_Decl1; use C83030c_Decl1;
package body C83030c_Decl2 is
   procedure C83030c_Proc1 is separate;
   procedure C83030c_Proc2 (X : T) is separate;
   function C83030c_Func3 return Integer is separate;
   function C83030c_Func4 return T is separate;
end C83030c_Decl2;

with Report;        use Report;
with C83030c_Decl1; use C83030c_Decl1;
package C83030c_Decl2 is
   generic
   procedure C83030c_Proc1;

   generic
      type T is (<>);
   procedure C83030c_Proc2 (X : T);

   generic
   function C83030c_Func3 return Integer;

   generic
      type T is (<>);
   function C83030c_Func4 return T;
end C83030c_Decl2;

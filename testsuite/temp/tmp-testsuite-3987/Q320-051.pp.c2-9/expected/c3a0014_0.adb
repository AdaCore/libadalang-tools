  --=======================================================================--

with Report;

package body C3a0014_0 is

   procedure Np_Proc (A : out Uc) is
   begin
      A := (3, "Bye");
   end Np_Proc;

   procedure Np_Cons (A : in out Uc; B : out Boolean) is
   begin
      B := A'Constrained;
   end Np_Cons;

   procedure P_Cons (A : out Auc; B : out Boolean) is
   begin
      B := A.all'Constrained;
   end P_Cons;

   package body Gen is

      procedure Proc is
      begin
         F := (2, "Fi");
      end Proc;

   end Gen;

   procedure Avoid_Optimization_And_Fail (P : Uc; Msg : String) is
      Default : Uc := (1, "!"); -- Unique value.
   begin
      if P = Default then       -- Both If branches can't do the same thing.
         Report.Failed (Msg & ": Constraint_Error not raised");
      else                      -- Subtests should always select this path.
         Report.Failed ("Constraint_Error not raised " & Msg);
      end if;
   end Avoid_Optimization_And_Fail;

end C3a0014_0;

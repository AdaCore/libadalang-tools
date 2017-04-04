     --==================================================================--

with Report;
package body C460004_0 is

   procedure Proc (X : in out Tag_Type) is
   begin
      X.C1 := 25;
   end Proc;

   -----------------------------------------
   procedure Proc (X : in out Dtag_Type) is
   begin
      Proc (Tag_Type (X));
      X.C2 := "Earth";
   end Proc;

   -----------------------------------------
   procedure Proc (X : in out Ddtag_Type) is
   begin
      Proc (Dtag_Type (X));
      X.C3 := "Orbit";
   end Proc;

   -----------------------------------------
   procedure Newproc (X : in Ddtag_Type) is
      Y : Ddtag_Type := X;
   begin
      Proc (Y);
   exception
      when others =>
         Report.Failed ("Unexpected exception in NewProc");
   end Newproc;

   -----------------------------------------
   function Cwfunc (X : Tag_Type'Class) return Tag_Type'Class is
      Y : Tag_Type'Class := X;
   begin
      Proc (Y);
      return Y;
   end Cwfunc;

end C460004_0;

     --==================================================================--

with Fc51a00;     -- Fraction type abstraction.
with Cc51a01_0;   -- Fraction I/O support.
with Cc51a01_1;   -- Positive fraction type abstraction.

with Report;
procedure Cc51a01 is

   type Distance is new Cc51a01_1.Pos_Fraction;    -- Derived indirectly from
   -- root type of class.
   -- Inherits "/" indirectly from root type. Inherits "-" indirectly from root
   -- type. Inherits Numerator directly from parent type. Inherits Denominator
   -- indirectly from root type.

   use Fc51a00, Cc51a01_1;                         -- All primitive subprograms
   -- directly visible.

   package Fraction_Io is new Cc51a01_0 (Fraction_Type);
   package Pos_Fraction_Io is new Cc51a01_0 (Pos_Fraction);
   package Distance_Io is new Cc51a01_0 (Distance);

   -- For each of the instances above, the subprogram "Put" should produce
   -- the same result. That is, the primitive subprograms called by Put should
   -- in all cases be those of the type Fraction_Type, which is the ancestor
   -- type for the formal derived type in the generic unit. In particular, for
   -- Pos_Fraction_IO and Distance_IO, the versions of Numerator called should
   -- NOT be those of the actual types, which override Fraction_Type's version.

   Tc_Expected_Result : constant String := "-3/ 16";

   Tc_Root_Type_Of_Class  : Fraction_Type := -3 / 16;
   Tc_Direct_Derivative   : Pos_Fraction  := -3 / 16;
   Tc_Indirect_Derivative : Distance      := -3 / 16;

begin
   Report.Test
     ("CC51A01",
      "Check that, in an instance, each implicit " &
      "declaration of a user-defined subprogram of a formal " &
      "derived record type declares a view of the corresponding " &
      "primitive subprogram of the ancestor, even if the " &
      "primitive subprogram has been overridden for the actual " &
      "type");

   if (Fraction_Io.Put (Tc_Root_Type_Of_Class) /= Tc_Expected_Result) then
      Report.Failed ("Wrong result for root type");
   end if;

   if (Pos_Fraction_Io.Put (Tc_Direct_Derivative) /= Tc_Expected_Result) then
      Report.Failed ("Wrong result for direct derivative");
   end if;

   if (Distance_Io.Put (Tc_Indirect_Derivative) /= Tc_Expected_Result) then
      Report.Failed ("Wrong result for INdirect derivative");
   end if;

   Report.Result;
end Cc51a01;

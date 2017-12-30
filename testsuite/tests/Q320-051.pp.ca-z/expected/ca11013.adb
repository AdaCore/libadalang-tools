--==================================================================--

with Report;
with Ca11013_2;                       -- Random number generator.
with Ca11013_3.Ca11013_4;             -- Complex abstraction + Random complex
-- number operation.
procedure Ca11013 is

   package My_Complex_Pkg renames Ca11013_3;
   use type Ca11013_2.My_Float;

   My_Complex                 : My_Complex_Pkg.Complex_Type;
   My_Literal                 : Ca11013_2.My_Float := 3.0;
   My_Real_Part, My_Imag_Part : Ca11013_2.My_Float;

begin

   Report.Test
     ("CA11013",
      "Check that child instance can use its parent's " &
      "declarations and operations, including a formal " &
      "subprogram of the parent");

   My_Complex := Ca11013_3.Ca11013_4 (My_Literal);
   -- Operation from the generic child function.

   My_Complex_Pkg.Components (My_Complex, My_Real_Part, My_Imag_Part);
   -- Operation from the generic parent package.

   if My_Real_Part /= 6.0           -- Operation from the generic
   or
     My_Imag_Part /= 9.0         -- parent package.
   then
      Report.Failed ("Incorrect results from complex operation");
   end if;

   Report.Result;

end Ca11013;

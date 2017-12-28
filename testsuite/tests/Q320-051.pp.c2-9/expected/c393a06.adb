with Report;

with C393a06_0;
with F393a00_0;
with F393a00_1;
procedure C393a06 is

   package Darwin renames C393a06_0;
   package Tagger renames F393a00_0;
   package Objects renames F393a00_1;

   Lion      : Darwin.Organism;
   Tigerlily : Darwin.Organism;
   Bear      : Darwin.Organism'Class := Darwin.Create;
   Sunflower : Darwin.Organism'Class := Darwin.Create;

   use type Darwin.Kingdoms;

begin  -- Main test procedure.

   Report.Test
     ("C393A06",
      "Check that a type that inherits abstract " &
      "operations but overrides each of these " &
      "operations is not required to be abstract. " &
      "Check that objects of the type and its " &
      "class-wide type may be declared and passed " &
      "in calls to the overriding subprograms");

   Tagger.Tc_Validate ("BaBa", "Declaration Initializations");

   Darwin.Initialize (Lion, Darwin.Animal);
   Darwin.Initialize (Tigerlily, Darwin.Vegetable);
   Darwin.Initialize (Bear, Darwin.Animal);
   Darwin.Initialize (Sunflower, Darwin.Vegetable);

   Tagger.Tc_Validate ("CaCaCaCa", "Initialization sequence");

   Oh_My :
   begin
      Darwin.Swap (Lion, Darwin.Organism (Bear));
      Darwin.Swap (Lion, Tigerlily);
      Report.Failed ("Exception not raised");
   exception
      when Darwin.Incompatible =>
         null;
   end Oh_My;

   Tagger.Tc_Validate ("AAX", "Swap sequence");

   if Darwin.Kingdom (Darwin.Create) = Darwin.Unspecified then
      Darwin.Swap (Sunflower, Darwin.Organism'Class (Tigerlily));
   end if;

   Tagger.Tc_Validate ("BaDA", "Vegetable swap sequence");

   Darwin.Tc_Check (Lion, Darwin.Animal, True);
   Darwin.Tc_Check (Tigerlily, Darwin.Vegetable, True);
   Darwin.Tc_Check (Bear, Darwin.Animal, True);
   Darwin.Tc_Check (Sunflower, Darwin.Vegetable, True);

   Tagger.Tc_Validate ("b+b+b+b+", "Final sequence");

   Report.Result;

end C393a06;

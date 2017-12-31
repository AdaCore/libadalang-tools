with Report;
with F393a00_0;
with C393a05_0;
procedure C393a05 is

   package Tracer renames F393a00_0;
   package Coffee renames C393a05_0;
   use type Coffee.Coarseness;

   Morning   : Coffee.Grinder;
   Afternoon : Coffee.Grinder;

   Gritty : Coffee.Coarseness;

   procedure Class_Swap (A, B : in out Coffee.Grinder'Class) is
   begin
      Coffee.Swap (A, B); -- dispatch
   end Class_Swap;

begin  -- Main test procedure.

   Report.Test
     ("C393A05",
      "Check that nonabstract private extensions, " &
      "inherited abstract subprograms overridden " &
      "in the private part can be dispatched from " & "outside the package");

   Tracer.Tc_Validate ("hh", "Declarations");

   Morning := Coffee.Create;
   Tracer.Tc_Validate ("hDa", "Creating Morning Coffee");
   Gritty := Coffee.Grind (Morning);
   Tracer.Tc_Validate ("B", "Finding Morning Grind");

   Afternoon := Coffee.Create;
   Tracer.Tc_Validate ("hDa", "Creating Afternoon Coffee");
   Coffee.Set_Grind (Afternoon, Coffee.Medium);
   Tracer.Tc_Validate ("A", "Setting Afternoon Grind");

   Coffee.Swap (Morning, Afternoon);
   Tracer.Tc_Validate ("C", "Dispatching Swapping Coffees");

   if Gritty /= Coffee.Grind (Afternoon) or
     Coffee.Grind (Afternoon) /= Coffee.Fine then
      Report.Failed ("Result of Swap");
   end if;
   Tracer.Tc_Validate ("BB", "Finding Afternoon Grind");

   Sunset :
   declare
      Evening : Coffee.Grinder'Class := Coffee.Create;
   begin
      Tracer.Tc_Validate ("hDa", "Creating Evening Coffee");

      Coffee.Set_Grind (Evening, Coffee.Espresso);
      Tracer.Tc_Validate ("A", "Setting Evening Grind");

      Morning := Coffee.Grinder (Evening);
      Class_Swap (Morning, Evening);
      Tracer.Tc_Validate ("C", "Swapping Coffees");
      if Coffee.Grind (Morning) /= Coffee.Espresso then
         Report.Failed ("Result of Assignment");
      end if;
   end Sunset;

   Report.Result;

end C393a05;

with C393011_0,
-- Definitions
 C393011_3;
-- New_Alert -- package Alert is not visible
with C393011_4;
with Report;
procedure C393011 is
   use C393011_4;
   use Definitions;

   Fa : Final_Alert_Type;

begin

   Report.Test
     ("C393011",
      "Check that an extended type can be derived " & "from an abstract type");

   if (Definitions.Display_Device /= Definitions.Bit_Bucket) or
     (Definitions.Next /= 1) or
     (Fa.Status /= Definitions.None) or
     (Fa.Serial_Num /= 0) or
     (Fa.Display_Dev /= Tty)
   then
      Report.Failed ("Incorrect initial conditions");
   end if;

   Handle (Fa);
   if (Definitions.Display_Device /= Definitions.Tty) or
     (Definitions.Next /= 2) or
     (Fa.Status /= Definitions.Handled) or
     (Fa.Serial_Num /= 1) or
     (Fa.Display_Dev /= Tty)
   then
      Report.Failed ("Incorrect results from Handle");
   end if;

   Report.Result;

end C393011;

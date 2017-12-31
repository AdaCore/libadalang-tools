-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- there is no package body CD30006_0

------------------------------------------------------------------- CD30006

with Report;
with Impdef;
with Cd30006_0;
with System.Storage_Elements;
procedure Cd30006 is

   My_Stuff : Cd30006_0.Some_Stuff;
   -- Impdef.Max_Default_Alignment

   My_Quarter : Cd30006_0.Quarter;
   -- CD30006_0.S_Units_per_Word / 2

   My_Half : Cd30006_0.Half;
   -- CD30006_0.S_Units_per_Word * 2

   Stuff_Object : Cd30006_0.O_Some_Stuff with
      Alignment => Impdef.Max_Default_Alignment;           -- ANX-C RQMT.

   Quarter_Object : Cd30006_0.O_Quarter with
      Alignment => Cd30006_0.Small_Alignment;              -- ANX-C RQMT.

   Half_Object : Cd30006_0.O_Half with
      Alignment => Cd30006_0.Multiple_Object_Alignment;    -- ANX-C RQMT.

   subtype Intadd is System.Storage_Elements.Integer_Address;
   use type System.Storage_Elements.Integer_Address;

   function A2i (Value : System.Address) return Intadd renames
     System.Storage_Elements.To_Integer;

   Nac : constant String := " not aligned correctly";

begin  -- Main test procedure.

   Report.Test
     ("CD30006",
      "Check that the implementation supports specifying " &
      "Alignments for subtypes and objects using aspect " & "notation");

   if A2i (Cd30006_0.Library_Level_Object'Address) mod
     Impdef.Max_Linker_Alignment /=
     0 then
      Report.Failed ("Library_Level_Object" & Nac);
   end if;

   if A2i (Cd30006_0.Delay_Object'Address) mod
     (Cd30006_0.Multiple_Object_Alignment) /=
     0 then
      Report.Failed ("Delayed multiple of words object" & Nac);
   end if;

   if A2i (My_Stuff'Address) mod Impdef.Max_Default_Alignment /= 0 then
      Report.Failed ("Max alignment subtype" & Nac);
   end if;

   if A2i (My_Quarter'Address) mod (Cd30006_0.Small_Alignment) /= 0 then
      Report.Failed ("Factor of words subtype" & Nac);
   end if;

   if A2i (My_Half'Address) mod (Cd30006_0.Multiple_Type_Alignment) /= 0 then
      Report.Failed ("Multiple of words subtype" & Nac);
   end if;

   if A2i (Stuff_Object'Address) mod Impdef.Max_Default_Alignment /= 0 then
      Report.Failed ("Stuff alignment object" & Nac);
   end if;

   if A2i (Quarter_Object'Address) mod (Cd30006_0.Small_Alignment) /= 0 then
      Report.Failed ("Factor of words object" & Nac);
   end if;

   if A2i (Half_Object'Address) mod (Cd30006_0.Multiple_Object_Alignment) /= 0
   then
      Report.Failed ("Multiple of words object" & Nac);
   end if;

   Report.Result;

end Cd30006;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- there is no package body CD30002_0

------------------------------------------------------------------- CD30002

with Report;
with Impdef;
with Cd30002_0;
with System.Storage_Elements;
procedure Cd30002 is

   My_Stuff : Cd30002_0.Some_Stuff;
   -- Impdef.Max_Default_Alignment

   My_Quarter : Cd30002_0.Quarter;
   -- CD30002_0.S_Units_per_Word / 2

   My_Half : Cd30002_0.Half;
   -- CD30002_0.S_Units_per_Word * 2

   Stuff_Object : Cd30002_0.O_Some_Stuff;
   for Stuff_Object'Alignment use Impdef
       .Max_Default_Alignment;                       -- ANX-C RQMT.

   Quarter_Object : Cd30002_0.O_Quarter;
   for Quarter_Object'Alignment use Cd30002_0
       .Small_Alignment;                        -- ANX-C RQMT.

   Half_Object : Cd30002_0.O_Half;
   for Half_Object'Alignment use Cd30002_0
       .Multiple_Object_Alignment;              -- ANX-C RQMT.

   subtype Intadd is System.Storage_Elements.Integer_Address;
   use type System.Storage_Elements.Integer_Address;

   function A2i
     (Value : System.Address) return Intadd renames
     System.Storage_Elements.To_Integer;

   Nac : constant String := " not aligned correctly";

begin  -- Main test procedure.

   Report.Test
     ("CD30002",
      "Check that the implementation supports " &
      "Alignments for subtypes and objects specified " &
      "as factors and multiples of the number of " &
      "storage elements per word, unless those values " &
      "cannot be loaded and stored. Check that the " &
      "largest alignment returned by default is " &
      "supported. Check that the implementation " &
      "supports Alignments supported by the target " &
      "linker for stand-alone library-level objects " &
      "of statically constrained subtypes");

   if A2i (Cd30002_0.Library_Level_Object'Address) mod
     Impdef.Max_Linker_Alignment /=
     0
   then
      Report.Failed ("Library_Level_Object" & Nac);
   end if;

   if A2i (My_Stuff'Address) mod Impdef.Max_Default_Alignment /= 0 then
      Report.Failed ("Max alignment subtype" & Nac);
   end if;

   if A2i (My_Quarter'Address) mod (Cd30002_0.Small_Alignment) /= 0 then
      Report.Failed ("Factor of words subtype" & Nac);
   end if;

   if A2i (My_Half'Address) mod (Cd30002_0.Multiple_Type_Alignment) /= 0 then
      Report.Failed ("Multiple of words subtype" & Nac);
   end if;

   if A2i (Stuff_Object'Address) mod Impdef.Max_Default_Alignment /= 0 then
      Report.Failed ("Stuff alignment object" & Nac);
   end if;

   if A2i (Quarter_Object'Address) mod (Cd30002_0.Small_Alignment) /= 0 then
      Report.Failed ("Factor of words object" & Nac);
   end if;

   if A2i (Half_Object'Address) mod (Cd30002_0.Multiple_Object_Alignment) /=
     0
   then
      Report.Failed ("Multiple of words object" & Nac);
   end if;

   Report.Result;

end Cd30002;

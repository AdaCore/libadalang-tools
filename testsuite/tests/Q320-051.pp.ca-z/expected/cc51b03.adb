     --==================================================================--

with Fc51b00;   -- Indefinite subtype declarations.
with Cc51b03_0; -- Generic package declarations.
with Cc51b03_1;

with Report;
procedure Cc51b03 is

   --
   -- Instances for formal private type with unknown discriminants:
   --

   package Privateformal_Undiscriminatedtaggedactual is new Cc51b03_0
     .Privateformalunknowndiscriminants
     (Fc51b00.Vector);

   package Privateformal_Classwideactual is new Cc51b03_0
     .Privateformalunknowndiscriminants
     (Fc51b00.Vector'Class);

   package Privateformal_Discriminatedtaggedactual is new Cc51b03_0
     .Privateformalunknowndiscriminants
     (Fc51b00.Square_Pair);

   package Privateformal_Discriminatedundefaultedrecordactual is new Cc51b03_0
     .Privateformalunknowndiscriminants
     (Fc51b00.Square);

   subtype Length is Natural range 0 .. 20;
   type Message (Len : Length := 0) is record   -- Record type with defaulted
      Text : String (1 .. Len);                 -- discriminant (definite).
   end record;

   package Privateformal_Discriminateddefaultedrecordactual is new Cc51b03_0
     .Privateformalunknowndiscriminants
     (Message);

   --
   -- Instances for formal derived tagged type with unknown discriminants:
   --

   package Derivedformal_Undiscriminatedtaggedactual is new Cc51b03_0
     .Taggedancestorunknowndiscriminants
     (Fc51b00.Vector);

   package Derivedformal_Classwideactual is new Cc51b03_0
     .Taggedancestorunknowndiscriminants
     (Fc51b00.Vector'Class);

   package Derivedformal_Discriminatedtaggedactual is new Cc51b03_0
     .Taggedancestorunknowndiscriminants
     (Cc51b03_1.Extended_Vector);

begin
   Report.Test
     ("CC51B03",
      "Check that S'Definite returns true if the " &
      "actual corresponding to S is definite, and false otherwise");

   if not Privateformal_Undiscriminatedtaggedactual.Is_Definite then
      Report.Failed
        ("Formal private/unknown discriminants: wrong " &
         "result for undiscriminated tagged actual");
   end if;

   if Privateformal_Classwideactual.Is_Definite then
      Report.Failed
        ("Formal private/unknown discriminants: wrong " &
         "result for class-wide actual");
   end if;

   if Privateformal_Discriminatedtaggedactual.Is_Definite then
      Report.Failed
        ("Formal private/unknown discriminants: wrong " &
         "result for discriminated tagged actual");
   end if;

   if Privateformal_Discriminatedundefaultedrecordactual.Is_Definite then
      Report.Failed
        ("Formal private/unknown discriminants: wrong result " &
         "for record actual with undefaulted discriminants");
   end if;

   if not Privateformal_Discriminateddefaultedrecordactual.Is_Definite then
      Report.Failed
        ("Formal private/unknown discriminants: wrong result " &
         "for record actual with defaulted discriminants");
   end if;

   if not Derivedformal_Undiscriminatedtaggedactual.Is_Definite then
      Report.Failed
        ("Formal derived/unknown discriminants: wrong result " &
         "for undiscriminated tagged actual");
   end if;

   if Derivedformal_Classwideactual.Is_Definite then
      Report.Failed
        ("Formal derived/unknown discriminants: wrong result " &
         "for class-wide actual");
   end if;

   if Derivedformal_Discriminatedtaggedactual.Is_Definite then
      Report.Failed
        ("Formal derived/unknown discriminants: wrong result " &
         "for discriminated tagged actual");
   end if;

   Report.Result;
end Cc51b03;

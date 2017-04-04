with C452001_0;
with C452001_1;
with C452001_2;
with C452001_3;
with C452001_4;
with C452001_5;
with Report;
procedure C452001 is

   Mars_Aphelion : C452001_1.Planet :=
     (Name           => "Mars           ",
      Representation =>
        (Center => (Report.Ident_Int (20), Report.Ident_Int (0)),
         Radius => Report.Ident_Int (4),
         Color  => C452001_0.Red));

   Mars_Perihelion : C452001_1.Planet :=
     (Name           => "Mars           ",
      Representation =>
        (Center => (Report.Ident_Int (-20), Report.Ident_Int (0)),
         Radius => Report.Ident_Int (4),
         Color  => C452001_0.Red));
   -- Mars_Perihelion = Mars_Aphelion if user-defined equality from
   -- the tagged type Colored_Circle was incorporated into
   -- predefined equality for the tagged type Planet. User-defined
   -- equality for Colored_Circle checks only that the Radii are equal.

   Blue_Mars : C452001_1.Planet :=
     (Name           => "Mars           ",
      Representation =>
        (Center => (Report.Ident_Int (10), Report.Ident_Int (10)),
         Radius => Report.Ident_Int (4),
         Color  => C452001_0.Blue));

   -- Blue_Mars should equal Mars_Perihelion, because Names and
   -- Radii are equal (all other components are not).

   Green_Mars : C452001_1.Planet :=
     (Name           => "Mars           ",
      Representation =>
        (Center => (Report.Ident_Int (10), Report.Ident_Int (10)),
         Radius => Report.Ident_Int (4),
         Color  => C452001_0.Green));
   -- Blue_Mars should equal Green_Mars. They differ only in the
   -- Color component. All user-defined equality operations return
   -- True, but records are not equal by predefined equality.

   -- Blue_Mars should equal Mars_Perihelion, because Names and
   -- Radii are equal (all other components are not).

   Moon_Craters : C452001_1.Craters :=
     ((Center => (Report.Ident_Int (9), Report.Ident_Int (11)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Black),
      (Center => (Report.Ident_Int (10), Report.Ident_Int (10)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Black),
      (Center => (Report.Ident_Int (11), Report.Ident_Int (9)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Black));

   Alternate_Moon_Craters : C452001_1.Craters :=
     ((Center => (Report.Ident_Int (9), Report.Ident_Int (9)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Yellow),
      (Center => (Report.Ident_Int (10), Report.Ident_Int (10)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Purple),
      (Center => (Report.Ident_Int (11), Report.Ident_Int (11)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Purple));
   -- Moon_Craters = Alternate_Moon_Craters if user-defined equality from
   -- the tagged type Colored_Circle was incorporated into
   -- predefined equality for the untagged type Craters. User-defined
   -- equality checks only that the Radii are equal.

   New_Moon : C452001_1.Moon :=
     (Name           => "Moon           ",
      Representation =>
        (Center => (Report.Ident_Int (10), Report.Ident_Int (8)),
         Radius => Report.Ident_Int (3),
         Color  => C452001_0.Black),
      Crater => Moon_Craters);

   Full_Moon : C452001_1.Moon :=
     (Name           => "Moon           ",
      Representation =>
        (Center => (Report.Ident_Int (10), Report.Ident_Int (8)),
         Radius => Report.Ident_Int (3),
         Color  => C452001_0.Black),
      Crater => Alternate_Moon_Craters);
   -- New_Moon = Full_Moon if user-defined equality from
   -- the tagged type Colored_Circle was incorporated into
   -- predefined equality for the untagged type Craters. This
   -- equality test should call user-defined equality for type
   -- TC_Planet (checks that Names are equal), then predefined
   -- equality for Craters (ultimately calls user-defined equality
   -- for type Circle, checking that Radii of craters are equal).

   Mars_Moon : C452001_1.Moon :=
     (Name           => "Phobos         ",
      Representation =>
        (Center => (Report.Ident_Int (10), Report.Ident_Int (8)),
         Radius => Report.Ident_Int (3),
         Color  => C452001_0.Black),
      Crater => Alternate_Moon_Craters);
   -- Mars_Moon /= Full_Moon since the Names differ.

   Alternate_Moon_Craters_2 : C452001_1.Craters :=
     ((Center => (Report.Ident_Int (10), Report.Ident_Int (10)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Red),
      (Center => (Report.Ident_Int (9), Report.Ident_Int (9)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Red),
      (Center => (Report.Ident_Int (10), Report.Ident_Int (9)),
       Radius => Report.Ident_Int (1),
       Color  => C452001_0.Red));

   Harvest_Moon : C452001_1.Moon :=
     (Name           => "Moon           ",
      Representation =>
        (Center => (Report.Ident_Int (11), Report.Ident_Int (7)),
         Radius => Report.Ident_Int (4),
         Color  => C452001_0.Orange),
      Crater => Alternate_Moon_Craters_2);
   -- For Harvest_Moon, only the fields that are employed by the
   -- user-defined equality operators are the same. Everything else
   -- differs. Equality should still return True.

   Viking_1_Orbiter : C452001_2.Mission :=
     (Craft =>
        (1 =>
           (Design      => C452001_2.Viking,
            Operational => Report.Ident_Bool (False))),
      Launch_Date => 1_975);

   Viking_1_Lander : C452001_2.Mission :=
     (Craft =>
        (1 =>
           (Design      => C452001_2.Viking,
            Operational => Report.Ident_Bool (True))),
      Launch_Date => 1_975);
   -- Viking_1_Orbiter /= Viking_1_Lander if predefined equality
   -- from the array type Spacecraft is used for equality
   -- of matching components in type Mission. If user-defined
   -- equality for type Spacecraft is incorporated, which it
   -- should not be by 4.5.2(24/3), then Viking_1_Orbiter = Viking_1_Lander.

   Voyagers : C452001_2.Inventory (1 .. 2) :=
     ((1 => (C452001_2.Voyager, Operational => Report.Ident_Bool (True))),
      (1 => (C452001_2.Voyager, Operational => Report.Ident_Bool (False))));

   Jupiter_Craft : C452001_2.Inventory (1 .. 2) :=
     ((1 => (C452001_2.Voyager, Operational => Report.Ident_Bool (True))),
      (1 => (C452001_2.Voyager, Operational => Report.Ident_Bool (True))));
   -- Voyagers /= Jupiter_Craft if predefined equality
   -- from the array type Spacecraft is used for equality
   -- of matching components in type Inventory. If user-defined
   -- equality for type Spacecraft is incorporated, which it
   -- should not be by 4.5.2(24/3), then Voyagers = Jupiter_Craft.

   Sun_Today : C452001_3.Star_Data :=
     (Item =>
        (Class => C452001_3.Normal, Color => C452001_3.Yellow, Planets => 8),
      Supernova => False);

   Sun_At_Bicentennial : C452001_3.Star_Data :=
     (Item =>
        (Class => C452001_3.Normal, Color => C452001_3.Yellow, Planets => 9),
      Supernova => False);
   -- Sun_Today /= Sun_at_Bicentennial if predefined equality
   -- from the untagged record type Star is used for equality
   -- of matching components in type Star_Data. If user-defined
   -- equality for type Star is incorporated, which it should
   -- be by 4.5.2(24/3), then Sun_Today = Sun_at_Bicentennial.
   -- [Poor Pluto! - RLB]

   Little_Dipper : C452001_3.Constellation (1 .. 3) :=
     (1 =>
        (Class => C452001_3.Normal, Color => C452001_3.Yellow, Planets => 0),
      2 => (Class => C452001_3.Normal, Color => C452001_3.White, Planets => 0),
      3 =>
        (Class   => C452001_3.Supergiant,
         Color   => C452001_3.White,
         Planets => 3));

   Ursa_Minor : C452001_3.Constellation (1 .. 3) :=
     (1 =>
        (Class => C452001_3.Normal, Color => C452001_3.Yellow, Planets => 1),
      2 => (Class => C452001_3.Normal, Color => C452001_3.White, Planets => 0),
      3 =>
        (Class   => C452001_3.Supergiant,
         Color   => C452001_3.White,
         Planets => 4));
   -- Little_Dipper /= Ursa_Minor if predefined equality
   -- from the untagged record type Star is used for equality
   -- of matching components in type Constellation. If user-defined
   -- equality for type Star is incorporated, which it should be
   -- by 4.5.2(24/3), then Little_Dipper = Ursa_Minor.

   Tptf_1 : C452001_4.Tagged_Partial_Tagged_Full;
   Tptf_2 : C452001_4.Tagged_Partial_Tagged_Full;
   -- With differing values for Boolean component, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is tagged, primitive equality
   -- should be used.

   Uptf_1 : C452001_4.Untagged_Partial_Tagged_Full;
   Uptf_2 : C452001_4.Untagged_Partial_Tagged_Full;
   -- With differing values for Boolean component, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is tagged, primitive equality
   -- should be used.

   Upurf_1 : C452001_4.Untagged_Partial_Untagged_Record_Full;
   Upurf_2 : C452001_4.Untagged_Partial_Untagged_Record_Full;
   -- With differing values for Duration component, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is an untagged record type,
   -- primitive equality should be used (4.5.2(15/3)).

   Upuaf_1 : C452001_4.Untagged_Partial_Untagged_Array_Full;
   Upuaf_2 : C452001_4.Untagged_Partial_Untagged_Array_Full;
   -- With differing values for components 6 through 12, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is an untagged array, predefined
   -- equality should be used.

   -- Test encorporation of "=" for private types.

   Tptf_Rec_1 : C452001_5.Tptf_Rec;
   Tptf_Rec_2 : C452001_5.Tptf_Rec;
   -- With differing values for Boolean component, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is tagged, primitive equality
   -- should be used.

   Uptf_Rec_1 : C452001_5.Uptf_Rec;
   Uptf_Rec_2 : C452001_5.Uptf_Rec;
   -- With differing values for Boolean component, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is tagged, primitive equality
   -- should be used.

   Upurf_Rec_1 : C452001_5.Upurf_Rec;
   Upurf_Rec_2 : C452001_5.Upurf_Rec;
   -- With differing values for Duration component, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is an untagged record type,
   -- primitive equality should be used (4.5.2(15/3)).

   Upuaf_Rec_1 : C452001_5.Upuaf_Rec;
   Upuaf_Rec_2 : C452001_5.Upuaf_Rec;
   -- With differing values for components 6 through 8, user-defined
   -- (primitive) equality returns True, predefined equality
   -- returns False. Since full type is an untagged array, predefined
   -- equality should be used.

   -- Use type clauses make "=" and "/=" operators directly visible
   use type C452001_1.Planet;
   use type C452001_1.Craters;
   use type C452001_1.Moon;
   use type C452001_2.Mission;
   use type C452001_2.Inventory;
   use type C452001_3.Star_Data;
   use type C452001_3.Constellation;
   use type C452001_4.Tagged_Partial_Tagged_Full;
   use type C452001_4.Untagged_Partial_Tagged_Full;
   use type C452001_4.Untagged_Partial_Untagged_Record_Full;
   use type C452001_4.Untagged_Partial_Untagged_Array_Full;
   use type C452001_5.Tptf_Rec;
   use type C452001_5.Uptf_Rec;
   use type C452001_5.Upurf_Rec;
   use type C452001_5.Upuaf_Rec;

begin

   Report.Test
     ("C452001",
      "Equality of private types and " &
      "composite types with tagged components");

   -------------------------------------------------------------------
   -- Tagged type with tagged component.
   -------------------------------------------------------------------

   if not (Mars_Aphelion = Mars_Perihelion) then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined equality " &
         "for enclosing tagged record type");
   end if;

   if Mars_Aphelion /= Mars_Perihelion then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined inequality " &
         "for enclosing tagged record type");
   end if;

   if not (Blue_Mars = Mars_Perihelion) then
      Report.Failed
        ("Equality test for tagged record type " &
         "incorporates record components " &
         "other than those used by user-defined equality");
   end if;

   if Blue_Mars /= Mars_Perihelion then
      Report.Failed
        ("Inequality test for tagged record type " &
         "incorporates record components " &
         "other than those used by user-defined equality");
   end if;

   if Blue_Mars /= Green_Mars then
      Report.Failed
        ("Records are unequal even though they only differ " &
         "in a component not used by user-defined equality");
   end if;

   if not (Blue_Mars = Green_Mars) then
      Report.Failed
        ("Records are not equal even though they only differ " &
         "in a component not used by user-defined equality");
   end if;

   -------------------------------------------------------------------
   -- Untagged (array) type with tagged component.
   -------------------------------------------------------------------

   if not (Moon_Craters = Alternate_Moon_Craters) then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined equality " &
         "for enclosing array type");
   end if;

   if Moon_Craters /= Alternate_Moon_Craters then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined inequality " &
         "for enclosing array type");
   end if;

   -------------------------------------------------------------------
   -- Tagged type with untagged composite component. Untagged
   -- component itself has tagged components.
   -------------------------------------------------------------------
   if not (New_Moon = Full_Moon) then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined equality " &
         "for array component of tagged record type");
   end if;

   if New_Moon /= Full_Moon then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined inequality " &
         "for array component of tagged record type");
   end if;

   if Mars_Moon = Full_Moon then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined equality " &
         "for array component of tagged record type");
   end if;

   if not (Mars_Moon /= Full_Moon) then
      Report.Failed
        ("User-defined equality for tagged component " &
         "was not incorporated into predefined inequality " &
         "for array component of tagged record type");
   end if;

   if not (Harvest_Moon = Full_Moon) then
      Report.Failed
        ("Equality test for record with array of tagged " &
         "components incorporates record components " &
         "other than those used by user-defined equality");
   end if;

   if Harvest_Moon /= Full_Moon then
      Report.Failed
        ("Inequality test for record with array of tagged " &
         "components incorporates record components " &
         "other than those used by user-defined equality");
   end if;

   -------------------------------------------------------------------
   -- Untagged types with no tagged or (directly) record components.
   -------------------------------------------------------------------

   -- Record type

   if Viking_1_Orbiter = Viking_1_Lander then
      Report.Failed
        ("User-defined equality for untagged array " &
         "component was incorporated into predefined " &
         "equality for " &
         "untagged record type");
   end if;

   if not (Viking_1_Orbiter /= Viking_1_Lander) then
      Report.Failed
        ("User-defined equality for untagged array " &
         "component was incorporated into predefined " &
         "inequality for " &
         "untagged record type");
   end if;

   -- Array type

   if Voyagers = Jupiter_Craft then
      Report.Failed
        ("User-defined equality for untagged array " &
         "component was incorporated into predefined " &
         "equality for " &
         "array type");
   end if;

   if not (Voyagers /= Jupiter_Craft) then
      Report.Failed
        ("User-defined equality for untagged array " &
         "component was incorporated into predefined " &
         "inequality for " &
         "array type");
   end if;

   -------------------------------------------------------------------
   -- Untagged types with untagged record components.
   -------------------------------------------------------------------

   -- Record type

   if not (Sun_Today = Sun_At_Bicentennial) then
      Report.Failed
        ("User-defined equality for untagged record component " &
         "was not incorporated into predefined equality " &
         "for untagged record type");
   end if;

   if Sun_Today /= Sun_At_Bicentennial then
      Report.Failed
        ("User-defined equality for untagged record component " &
         "was not incorporated into predefined inequality " &
         "for untagged record type");
   end if;

   -- Array type

   if not (Little_Dipper = Ursa_Minor) then
      Report.Failed
        ("User-defined equality for untagged record component " &
         "was not incorporated into predefined equality " &
         "for array type");
   end if;

   if Little_Dipper /= Ursa_Minor then
      Report.Failed
        ("User-defined equality for untagged record component " &
         "was not incorporated into predefined inequality " &
         "for array type");
   end if;

   -------------------------------------------------------------------
   -- Private types tests.
   -------------------------------------------------------------------

   -- Make objects differ from one another

   C452001_4.Change (Tptf_1, False);
   C452001_4.Change (Uptf_1, 999);
   C452001_4.Change (Upurf_1, 40.0);
   C452001_4.Change (Upuaf_1, 'R');

   -------------------------------------------------------------------
   -- Partial type and full type are tagged. (Full type must be tagged
   -- if partial type is tagged)
   -------------------------------------------------------------------

   if not (Tptf_1 = Tptf_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "tagged private type " &
         "instead of user-defined (primitive) equality");
   end if;

   if Tptf_1 /= Tptf_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "tagged private type " &
         "instead of user-defined (primitive) equality");
   end if;

   -------------------------------------------------------------------
   -- Partial type untagged, full type tagged.
   -------------------------------------------------------------------

   if not (Uptf_1 = Uptf_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "private type (untagged partial view, " &
         "tagged full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   if Uptf_1 /= Uptf_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "private type (untagged partial view, " &
         "tagged full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   -------------------------------------------------------------------
   -- Partial type and full type are both untagged, the full type is
   -- a record type.
   -------------------------------------------------------------------

   -- Note: In the current test, primitive "=" is visible, so we need
   -- to use that. If 4.5.2(9.7/3) is repealed and the visible declarations
   -- removed, then this result doesn't change.

   if not (Upurf_1 = Upurf_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "private type (untagged partial view, " &
         "untagged record full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   if Upurf_1 /= Upurf_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "private type (untagged partial view, " &
         "untagged record full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   -------------------------------------------------------------------
   -- Partial type and full type are both untagged, the full type is
   -- an array type.
   -------------------------------------------------------------------

   -- Note: In the current test, primitive "=" is visible, so we need
   -- to use that. If 4.5.2(9.7/3) is repealed and visible declarations
   -- removed, then this result changes to the commented out version below.

   if not (Upuaf_1 = Upuaf_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "private type (untagged partial view, " &
         "untagged array full view) " &
         "instead of visible user-defined (primitive) equality");
   end if;

   if Upuaf_1 /= Upuaf_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "private type (untagged partial view, " &
         "untagged array full view) " &
         "instead of visible user-defined (primitive) equality");
   end if;

   --if UPUAF_1 = UPUAF_2 then
   --   Report.Failed ("User-defined (primitive) equality for full type " &
   --                  "was used to determine equality of " &
   --                  "private type (untagged partial view, " &
   --                  "untagged array full view) " &
   --                  "instead of predefined equality");
   --end if;
   --
   --if not (UPUAF_1 /= UPUAF_2) then
   --   Report.Failed ("User-defined (primitive) equality for full type " &
   --                  "was used to determine inequality of " &
   --                  "private type (untagged partial view, " &
   --                  "untagged array full view) " &
   --                  "instead of predefined equality");
   --end if;

   -------------------------------------------------------------------
   -- Private type incorporation tests.
   -------------------------------------------------------------------

   -- Make objects differ from one another

   C452001_4.Change (Tptf_Rec_1.Comp, False);
   C452001_4.Change (Uptf_Rec_1.Comp, 999);
   C452001_4.Change (Upurf_Rec_1.Comp, 40.0);
   C452001_4.Change (Upuaf_Rec_1.Comp, 'R');

   -------------------------------------------------------------------
   -- Partial type and full type are tagged. (Full type must be tagged
   -- if partial type is tagged)
   -------------------------------------------------------------------

   if not (Tptf_Rec_1 = Tptf_Rec_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "record containing component with tagged private type " &
         "instead of user-defined (primitive) equality");
   end if;

   if Tptf_Rec_1 /= Tptf_Rec_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "record containing component with tagged private type " &
         "instead of user-defined (primitive) equality");
   end if;

   -------------------------------------------------------------------
   -- Partial type untagged, full type tagged.
   -------------------------------------------------------------------

   if not (Uptf_Rec_1 = Uptf_Rec_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "record containing component with private type " &
         "(untagged partial view, tagged full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   if Uptf_Rec_1 /= Uptf_Rec_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "record containing component with private type " &
         "(untagged partial view, tagged full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   -------------------------------------------------------------------
   -- Partial type and full type are both untagged, the full type is
   -- a record type.
   -------------------------------------------------------------------

   if not (Upurf_Rec_1 = Upurf_Rec_2) then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine equality of " &
         "record containing component with private type " &
         "(untagged partial view, untagged record full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   if Upurf_Rec_1 /= Upurf_Rec_2 then
      Report.Failed
        ("Predefined equality for full type " &
         "was used to determine inequality of " &
         "record containing component with private type " &
         "(untagged partial view, untagged record full view) " &
         "instead of user-defined (primitive) equality");
   end if;

   -------------------------------------------------------------------
   -- Partial type and full type are both untagged, the full type is
   -- an array type.
   -------------------------------------------------------------------

   if Upuaf_Rec_1 = Upuaf_Rec_2 then
      Report.Failed
        ("User-defined (primitive) equality for full type " &
         "was used to determine equality of " &
         "record containing component with private type " &
         "(untagged partial view, untagged array full view) " &
         "instead of predefined equality");
   end if;

   if not (Upuaf_Rec_1 /= Upuaf_Rec_2) then
      Report.Failed
        ("User-defined (primitive) equality for full type " &
         "was used to determine inequality of " &
         "record containing component with private type " &
         "(untagged partial view, untagged array full view) " &
         "instead of predefined equality");
   end if;

   -------------------------------------------------------------------
   Report.Result;

end C452001;

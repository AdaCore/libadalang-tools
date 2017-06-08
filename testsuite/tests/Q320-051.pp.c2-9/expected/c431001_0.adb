with Report;
package body C431001_0 is

   function Summary (R : in Recording; Tc_Type : in Tc_Type_Id) return String
   is
   begin

      if Tc_Type /= Tc_Recording then
         Report.Failed
           ("Did not dispatch on tag for tagged parent " & "type Recording");
      end if;

      return R.Artist (1 .. 10) &
        ' ' &
        Genre'Image (R.Category) (1 .. 2) &
        ' ' &
        Duration'Image (R.Length) &
        ' ' &
        Integer'Image (R.Selections);

   end Summary;

   function Summary (Disc : in Cd; Tc_Type : in Tc_Type_Id) return String is
   begin

      if Tc_Type /= Tc_Cd then
         Report.Failed ("Did not dispatch on tag for type extension " & "CD");
      end if;

      return Summary (Recording (Disc), Tc_Type => Tc_Recording) &
        ' ' &
        Recording_Method'Image (Disc.Recorded) (1) &
        Recording_Method'Image (Disc.Mastered) (1);

   end Summary;

   function Summary (Album : in Vinyl; Tc_Type : in Tc_Type_Id) return String
   is
   begin
      if Tc_Type /= Tc_Vinyl then
         Report.Failed
           ("Did not dispatch on tag for type extension " & "Vinyl");
      end if;

      case Album.Speed is
         when Lp_33 =>
            return Summary (Recording (Album), Tc_Type => Tc_Recording) &
              " 33";
         when Single_45 =>
            return Summary (Recording (Album), Tc_Type => Tc_Recording) &
              " 45";
         when Old_78 =>
            return Summary (Recording (Album), Tc_Type => Tc_Recording) &
              " 78";
      end case;

   end Summary;

   function Summary (Disk : in Cd_Rom; Tc_Type : in Tc_Type_Id) return String
   is
   begin
      if Tc_Type /= Tc_Cd_Rom then
         Report.Failed
           ("Did not dispatch on tag for type extension " &
            "CD_ROM. This is an extension of the type " &
            "extension CD");
      end if;

      return Summary (Recording (Disk), Tc_Type => Tc_Recording) &
        ' ' &
        Integer'Image (Disk.Storage) &
        'K';

   end Summary;

   function Catalog_Entry
     (R       : in Recording'Class;
      Tc_Type : in Tc_Type_Id) return String
   is
   begin
      return Summary (R, Tc_Type); -- dispatched call
   end Catalog_Entry;

   procedure Print (S : in String) is
      T : String (1 .. S'Length) := Report.Ident_Str (S);
   begin
      -- Ada.Text_IO.Put_Line (S);
      null;
   end Print;

   -- Bodies for null type checks
   procedure Tc_Check (N : in Null_Tagged; Tc_Type : in Tc_N_Type_Id) is
   begin
      if Tc_Type /= Tc_Null_Tagged then
         Report.Failed
           ("Did not dispatch on tag for null tagged " & "type Null_Tagged");
      end if;
   end Tc_Check;

   procedure Tc_Check (N : in Null_Extension; Tc_Type : in Tc_N_Type_Id) is
   begin
      if Tc_Type /= Tc_Null_Extension then
         Report.Failed
           ("Did not dispatch on tag for null tagged " &
            "type extension Null_Extension");
      end if;
   end Tc_Check;

   procedure Tc_Check (N : in Extension_Of_Null; Tc_Type : in Tc_N_Type_Id) is
   begin
      if Tc_Type /= Tc_Extension_Of_Null then
         Report.Failed
           ("Did not dispatch on tag for extension of null parent" & "type");
      end if;
   end Tc_Check;

   procedure Tc_Check
     (N       : in Null_Extension_Of_Nonnull;
      Tc_Type : in Tc_N_Type_Id)
   is
   begin
      if Tc_Type /= Tc_Null_Extension_Of_Nonnull then
         Report.Failed
           ("Did not dispatch on tag for null extension of nonnull " &
            "parent type");
      end if;
   end Tc_Check;

   procedure Tc_Dispatch (N : in Null_Tagged'Class; Tc_Type : in Tc_N_Type_Id)
   is
   begin
      Tc_Check (N, Tc_Type); -- dispatched call
   end Tc_Dispatch;

end C431001_0;

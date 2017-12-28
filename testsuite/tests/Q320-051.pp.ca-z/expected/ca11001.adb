--=======================================================================--

with Ca11001_0.Ca11001_1;        -- Polar_Complex
with Report;

procedure Ca11001 is

   Complex_No : Ca11001_0.Complex_Type;    -- Complex_Type is a
   -- record of CA11001_0

   Complex_5x2 : Ca11001_0.Complex_Type := Ca11001_0.Complex (5, 2);

   Int_2 : Ca11001_0.Complex_Int :=
     Ca11001_0.Complex_Int (Report.Ident_Int (2));

begin

   Report.Test
     ("CA11001",
      "Check that a child unit can be used " &
      "to provide an alternate view and operations " &
      "on a private type in its parent package");

   Basic_View_Subtest :

   begin
      -- Assign using Cartesian coordinates.
      Ca11001_0.Cartesian_Assign
        (Ca11001_0.Complex_Int (Report.Ident_Int (1)),
         Int_2,
         Complex_No);

      -- Read back in Polar coordinates. Polar values are surrogates used in
      -- checking for correct subprogram calls.
      if Ca11001_0."/="
          (Ca11001_0.Ca11001_1.Polar_Real_Part (Complex_No),
           Ca11001_0.Cartesian_Real_Part (Complex_5x2)) and
        Ca11001_0."/="
          (Ca11001_0.Ca11001_1.Polar_Imag_Part (Complex_No),
           Ca11001_0.Cartesian_Imag_Part (Complex_5x2))
      then
         Report.Failed ("Incorrect Cartesian result");
      end if;

   end Basic_View_Subtest;
   -------------------------------------------------------------
   Alternate_View_Subtest :
   begin
      -- Assign using Polar coordinates.
      Ca11001_0.Ca11001_1.Polar_Assign
        (Int_2,
         Ca11001_0.Complex_Int (Report.Ident_Int (3)),
         Complex_No);

      -- Read back in Cartesian coordinates.
      if Ca11001_0."/="
          (Ca11001_0.Cartesian_Real_Part (Complex_No),
           Ca11001_0.Complex_Int (Report.Ident_Int (12))) or
        Ca11001_0."/=" (Ca11001_0.Cartesian_Imag_Part (Complex_No), Int_2)
      then
         Report.Failed ("Incorrect Polar result");
      end if;
   end Alternate_View_Subtest;
   -------------------------------------------------------------
   Other_Subtest :
   begin
      -- Assign using Polar coordinates.
      Ca11001_0.Ca11001_1.Polar_Assign
        (Ca11001_0.Complex_Int (Report.Ident_Int (0)),
         Int_2,
         Complex_No);

      -- Compare with Complex_Num in CA11001_0.
      if not Ca11001_0.Ca11001_1.Equals_Const (Complex_No) then
         Report.Failed ("Incorrect result");
      end if;
   end Other_Subtest;
   -------------------------------------------------------------
   Exception_Subtest :
   begin
      -- Raised parent's exception.
      Ca11001_0.Ca11001_1.Polar_Assign
        (Ca11001_0.Complex_Int (Report.Ident_Int (0)),
         Ca11001_0.Complex_Int (Report.Ident_Int (0)),
         Complex_No);
      Report.Failed ("Exception was not raised");
   exception
      when Ca11001_0.Complex_Error =>
         null;
      when others =>
         Report.Failed ("Unexpected exception raised in test");
   end Exception_Subtest;

   Report.Result;

end Ca11001;

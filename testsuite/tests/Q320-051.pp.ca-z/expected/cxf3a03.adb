-- CXF3A03.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that function Length in the generic package Decimal_Output
--      returns the number of characters in the edited output string
--      produced by function Image, for a particular decimal type,
--      currency string, and radix mark.
--      Check that function Valid in the generic package Decimal_Output
--      returns correct results based on the particular decimal value,
--      and the Picture and Currency string parameters.
--
-- TEST DESCRIPTION:
--      This test uses two instantiations of package Decimal_Output, one
--      for decimal data with delta 0.01, the other for decimal data with
--      delta 1.0. The functions Length and Valid found in this generic
--      package are evaluated for each instantiation.
--      Function Length is examined with picture and currency string input
--      parameters of different sizes.
--      Function Valid is examined with a decimal type data item, picture
--      object, and currency string, for cases that are both valid and
--      invalid (Layout_Error would result from the particular items as
--      input parameters to function Image).
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A03.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Fxf3a00;
with Ada.Text_Io.Editing;
with Report;

procedure Cxf3a03 is
begin

   Report.Test
     ("CXF3A03",
      "Check that function Length returns the " &
      "number of characters in the edited output " &
      "string produced by function Image, for a " &
      "particular decimal type, currency string, " &
      "and radix mark.  Check that function Valid " &
      "returns correct results based on the " &
      "particular decimal value, and the Picture " &
      "and Currency string parameters");

   Test_Block : declare

      use Ada.Text_Io;
      use Fxf3a00;

      type Instantiation_Type is (Ndp, Twodp);

      -- Defaults used for all other generic parameters in these
      -- instantiations.
      package Pack_Ndp is new Editing.Decimal_Output (Decimal_Type_Ndp);
      package Pack_2dp is new Editing.Decimal_Output (Decimal_Type_2dp);

      Tc_Lower_Bound, Tc_Higher_Bound : Integer := 0;

      Tc_Picture    : Editing.Picture;
      Tc_Us_String  : constant String := "$";
      Tc_Ff_String  : constant String := "FF";
      Tc_Dm_String  : constant String := "DM";
      Tc_Chf_String : constant String := "CHF";

      function Dollar_Sign_Present (Str : String) return Boolean is
      begin
         for I in 1 .. Str'Length loop
            if Str (I) = '$' then
               return True;
            end if;
         end loop;
         return False;
      end Dollar_Sign_Present;

      function V_Present (Str : String) return Boolean is
      begin
         for I in 1 .. Str'Length loop
            if Str (I) = 'V' or Str (I) = 'v' then
               return True;
            end if;
         end loop;
         return False;
      end V_Present;

      function Accurate_Length
        (Pict_Str        : String;
         Inst            : Instantiation_Type;
         Currency_String : String) return Boolean
      is

         Tc_Length                     : Natural := 0;
         Tc_Currency_Length_Adjustment : Natural := 0;
         Tc_Radix_Adjustment           : Natural := 0;
      begin

         -- Create the picture object from the picture string.
         Tc_Picture := Editing.To_Picture (Pict_Str);

         -- Calculate the currency length adjustment.
         if Dollar_Sign_Present (Editing.Pic_String (Tc_Picture)) then
            Tc_Currency_Length_Adjustment := Currency_String'Length - 1;
         end if;

         -- Calculate the Radix adjustment.
         if V_Present (Editing.Pic_String (Tc_Picture)) then
            Tc_Radix_Adjustment := 1;
         end if;

         -- Calculate the length, using the version of Length that comes
         -- from the appropriate instantiation of Decimal_Output, based
         -- on the decimal type used in the instantiation.
         if Inst = Ndp then
            Tc_Length := Pack_Ndp.Length (Tc_Picture, Currency_String);
         else
            Tc_Length := Pack_2dp.Length (Tc_Picture, Currency_String);
         end if;

         return Tc_Length =
           Editing.Pic_String (Tc_Picture)'Length +
             Tc_Currency_Length_Adjustment -
             Tc_Radix_Adjustment;
      end Accurate_Length;

   begin

      Length_Block : begin

         -- The first 10 picture strings in the Valid_Strings array correspond
         -- to data values of a decimal type with delta 0.01.
         -- Note: The appropriate instantiation of the Decimal_Output package
         --       (and therefore function Length) is used by function
         --       Accurate_Length to calculate length.

         for I in 1 .. 10 loop
            if not Accurate_Length
                (Fxf3a00.Valid_Strings (I).all,
                 Twodp,
                 Tc_Us_String)
            then
               Report.Failed
                 ("Incorrect result from function Length, " &
                  "when used with a decimal type with delta .01 " &
                  "and with the currency string " &
                  Tc_Us_String &
                  " in evaluating picture string " &
                  Fxf3a00.Valid_Strings (I).all);
            end if;
         end loop;

         -- Picture strings 17-20 in the Valid_Strings array correspond
         -- to data values of a decimal type with delta 1.0.  Again, the
         -- instantiation of Decimal_Output used is based on this particular
         -- decimal type.

         for I in 17 .. 20 loop
            if not Accurate_Length
                (Fxf3a00.Valid_Strings (I).all,
                 Ndp,
                 Tc_Us_String)
            then
               Report.Failed
                 ("Incorrect result from function Length, " &
                  "when used with a decimal type with delta 1.0 " &
                  "and with the currency string " &
                  Tc_Us_String &
                  " in evaluating picture string " &
                  Fxf3a00.Valid_Strings (I).all);
            end if;
         end loop;

         -- The first 4 picture strings in the Foreign_Strings array
         -- correspond to data values of a decimal type with delta 0.01,
         -- and to the currency string "FF" (two characters).

         for I in 1 .. Fxf3a00.Number_Of_Ff_Strings loop
            if not Accurate_Length
                (Fxf3a00.Foreign_Strings (I).all,
                 Twodp,
                 Tc_Ff_String)
            then
               Report.Failed
                 ("Incorrect result from function Length, " &
                  "when used with a decimal type with delta .01 " &
                  "and with the currency string " &
                  Tc_Ff_String &
                  " in evaluating picture string " &
                  Fxf3a00.Foreign_Strings (I).all);
            end if;
         end loop;

         -- Picture strings 5-9 in the Foreign_Strings array correspond
         -- to data values of a decimal type with delta 0.01, and to the
         -- currency string "DM" (two characters).

         Tc_Lower_Bound  := Fxf3a00.Number_Of_Ff_Strings + 1;
         Tc_Higher_Bound :=
           Fxf3a00.Number_Of_Ff_Strings + Fxf3a00.Number_Of_Dm_Strings;

         for I in Tc_Lower_Bound .. Tc_Higher_Bound loop
            if not Accurate_Length
                (Fxf3a00.Foreign_Strings (I).all,
                 Twodp,
                 Tc_Dm_String)
            then
               Report.Failed
                 ("Incorrect result from function Length, " &
                  "when used with a decimal type with delta .01 " &
                  "and with the currency string " &
                  Tc_Dm_String &
                  " in evaluating picture string " &
                  Fxf3a00.Foreign_Strings (I).all);
            end if;
         end loop;

         -- Picture string #10 in the Foreign_Strings array corresponds
         -- to a data value of a decimal type with delta 0.01, and to the
         -- currency string "CHF" (three characters).

         if not Accurate_Length
             (Fxf3a00.Foreign_Strings (10).all,
              Twodp,
              Tc_Chf_String)
         then
            Report.Failed
              ("Incorrect result from function Length, " &
               "when used with a decimal type with delta .01 " &
               "and with the currency string " &
               Tc_Chf_String);
         end if;

      exception
         when others =>
            Report.Failed ("Unexpected exception raised in Length_Block");
      end Length_Block;

      Valid_Block : declare

         -- This offset value is used to align picture string and decimal
         -- data values from package FXF3A00 for proper correspondence for
         -- the evaluations below.

         Tc_Offset : constant Natural := 10;

      begin

         -- The following four For Loops examine cases where the
         -- decimal data/picture string/currency combinations used will
         -- generate valid Edited Output strings.  These combinations, when
         -- provided to the Function Valid (from instantiations of
         -- Decimal_Output), should result in a return result of True.
         -- The particular instantiated version of Valid used in these loops
         -- is that for decimal data with delta 0.01.

         -- The first 4 picture strings in the Foreign_Strings array
         -- correspond to data values of a decimal type with delta 0.01,
         -- and to the currency string "FF" (two characters).

         for I in 1 .. Fxf3a00.Number_Of_Ff_Strings loop
            -- Create the picture object from the picture string.
            Tc_Picture := Editing.To_Picture (Fxf3a00.Foreign_Strings (I).all);

            if not Pack_2dp.Valid
                (Fxf3a00.Data_With_2dp (Tc_Offset + I),
                 Tc_Picture,
                 Tc_Ff_String)
            then
               Report.Failed
                 ("Incorrect result from function Valid, " &
                  "when used with a decimal type with delta .01 " &
                  "and with the currency string " &
                  Tc_Ff_String &
                  " in evaluating picture string " &
                  Fxf3a00.Foreign_Strings (I).all);
            end if;
         end loop;

         -- Picture strings 5-9 in the Foreign_Strings array correspond
         -- to data values of a decimal type with delta 0.01, and to the
         -- currency string "DM" (two characters).

         Tc_Lower_Bound  := Fxf3a00.Number_Of_Ff_Strings + 1;
         Tc_Higher_Bound :=
           Fxf3a00.Number_Of_Ff_Strings + Fxf3a00.Number_Of_Dm_Strings;

         for I in Tc_Lower_Bound .. Tc_Higher_Bound loop
            -- Create the picture object from the picture string.
            Tc_Picture := Editing.To_Picture (Fxf3a00.Foreign_Strings (I).all);

            if not Pack_2dp.Valid
                (Fxf3a00.Data_With_2dp (Tc_Offset + I),
                 Tc_Picture,
                 Tc_Dm_String)
            then
               Report.Failed
                 ("Incorrect result from function Valid, " &
                  "when used with a decimal type with delta .01 " &
                  "and with the currency string " &
                  Tc_Dm_String &
                  " in evaluating picture string " &
                  Fxf3a00.Foreign_Strings (I).all);
            end if;
         end loop;

         -- Picture string #10 in the Foreign_Strings array corresponds
         -- to a data value of a decimal type with delta 0.01, and to the
         -- currency string "CHF" (three characters).

         -- Create the picture object from the picture string.
         Tc_Picture := Editing.To_Picture (Fxf3a00.Foreign_Strings (10).all);

         if not Pack_2dp.Valid
             (Fxf3a00.Data_With_2dp (Tc_Offset + 10),
              Tc_Picture,
              Tc_Chf_String)
         then
            Report.Failed
              ("Incorrect result from function Valid, " &
               "when used with a decimal type with delta .01 " &
               "and with the currency string " &
               Tc_Chf_String);
         end if;

         -- The following For Loop examines cases where the
         -- decimal data/picture string/currency combinations used will
         -- generate valid Edited Output strings.
         -- The particular instantiated version of Valid used in this loop
         -- is that for decimal data with delta 1.0; the others above have
         -- been for decimal data with delta 0.01.
         -- Note: TC_Offset is used here to align picture strings from the
         --       FXF3A00.Valid_Strings table with the appropriate decimal
         --       data in the FXF3A00.Data_With_NDP table.

         for I in 1 .. Fxf3a00.Number_Of_Ndp_Items loop
            -- Create the picture object from the picture string.
            Tc_Picture :=
              Editing.To_Picture (Fxf3a00.Valid_Strings (Tc_Offset + I).all);

            if not Pack_Ndp.Valid
                (Fxf3a00.Data_With_Ndp (I),
                 Tc_Picture,
                 Tc_Us_String)
            then
               Report.Failed
                 ("Incorrect result from function Valid, " &
                  "when used with a decimal type with delta .01 " &
                  "and with the currency string " &
                  Tc_Us_String &
                  " in evaluating picture string " &
                  Fxf3a00.Valid_Strings (I).all);
            end if;
         end loop;

         -- The following three evaluations of picture strings, used in
         -- conjunction with the specific decimal values provided, will cause
         -- Editing.Image to raise Layout_Error (to be examined in other
         -- tests).  Function Valid should return a False result for these
         -- combinations.
         -- The first two evaluations use the instantiation of Decimal_Output
         -- with a decimal type with delta 0.01, while the last evaluation
         -- uses the instantiation with decimal type with delta 1.0.

         for I in 1 .. Fxf3a00.Number_Of_Erroneous_Conditions loop

            -- Create the picture object from the picture string.
            Tc_Picture :=
              Editing.To_Picture (Fxf3a00.Erroneous_Strings (I).all);

            if I < 3 then  -- Choose the appropriate instantiation.
               if Pack_2dp.Valid
                   (Item     => Fxf3a00.Erroneous_Data (I),
                    Pic      => Tc_Picture,
                    Currency => Tc_Us_String)
               then
                  Report.Failed
                    ("Incorrect result from function Valid, " &
                     "when used with a decimal type with delta " &
                     "0.01 and with the currency string " &
                     Tc_Us_String &
                     " in evaluating picture string " &
                     Fxf3a00.Valid_Strings (I).all);
               end if;
            else
               if Pack_Ndp.Valid
                   (Item =>
                      Fxf3a00.Decimal_Type_Ndp (Fxf3a00.Erroneous_Data (I)),
                    Pic      => Tc_Picture,
                    Currency => Tc_Us_String)
               then
                  Report.Failed
                    ("Incorrect result from function Valid, " &
                     "when used with a decimal type with delta " &
                     "1.0 and with the currency string " &
                     Tc_Us_String &
                     " in evaluating picture string " &
                     Fxf3a00.Valid_Strings (I).all);
               end if;
            end if;
         end loop;

      exception
         when others =>
            Report.Failed ("Unexpected exception raised in Valid_Block");
      end Valid_Block;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxf3a03;

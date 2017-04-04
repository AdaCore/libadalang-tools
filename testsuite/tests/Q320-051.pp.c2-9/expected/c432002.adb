with C432002_0;
with Report;
procedure C432002 is

   -- Various different-sized strings for variety
   String_3  : String (1 .. 3)  := Report.Ident_Str ("123");
   String_5  : String (1 .. 5)  := Report.Ident_Str ("12345");
   String_8  : String (1 .. 8)  := Report.Ident_Str ("12345678");
   String_10 : String (1 .. 10) := Report.Ident_Str ("1234567890");
   String_11 : String (1 .. 11) := Report.Ident_Str ("12345678901");
   String_20 : String (1 .. 20) := Report.Ident_Str ("12345678901234567890");

begin

   Report.Test ("C432002", "Extension aggregates for discriminated types");

   --------------------------------------------------------------------
   -- Extension constrains parent's discriminant to value of expression
   --------------------------------------------------------------------

   -- Successful cases - value matches corresponding discriminant value

   Cd_Matched_Aggregate : begin
      declare
         Cd : C432002_0.Constrained_Discriminant_Extension :=
           (C432002_0.Discriminant'(L => 10, S1 => String_10) with
            S2 => String_20);
      begin
         C432002_0.Do_Something (Cd); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension " &
            "with discriminant constrained: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Cd_Matched_Aggregate;

   Cd_Matched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 10) :=
           C432002_0.Discriminant'(L => 10, S1 => String_10);

         Cd : C432002_0.Constrained_Discriminant_Extension :=
           (D with S2 => String_20);
      begin
         C432002_0.Do_Something (Cd); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is a variable");
         Report.Failed
           ("Aggregate of extension " &
            "with discriminant constrained: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Cd_Matched_Variable;

   -- Unsuccessful cases - value does not match value of corresponding
   --                      discriminant. Constraint_Error should be
   --                      raised.

   Cd_Unmatched_Aggregate : begin
      declare
         Cd : C432002_0.Constrained_Discriminant_Extension :=
           (C432002_0.Discriminant'(L => 5, S1 => String_5) with
            S2 => String_20);
      begin
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension " &
            "with discriminant constrained: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Cd); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise of Constraint_Error is expected
   end Cd_Unmatched_Aggregate;

   Cd_Unmatched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 5) :=
           C432002_0.Discriminant'(L => 5, S1 => String_5);

         Cd : C432002_0.Constrained_Discriminant_Extension :=
           (D with S2 => String_20);
      begin
         Report.Comment ("Ancestor expression is an variable");
         Report.Failed
           ("Aggregate of extension " &
            "with discriminant constrained: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Cd); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise of Constraint_Error is expected
   end Cd_Unmatched_Variable;

   -----------------------------------------------------------------------
   -- Extension constrains parent's discriminant to equal new discriminant
   -----------------------------------------------------------------------

   -- Successful cases - value matches corresponding discriminant value

   Nd_Matched_Aggregate : begin
      declare
         Nd : C432002_0.New_Discriminant_Extension (N => 8) :=
           (C432002_0.Discriminant'(L => 8, S1 => String_8) with
            N  => 8,
            S2 => String_8);
      begin
         C432002_0.Do_Something (Nd); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension " &
            "with new discriminant: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Nd_Matched_Aggregate;

   Nd_Matched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 3) :=
           C432002_0.Discriminant'(L => 3, S1 => String_3);

         Nd : C432002_0.New_Discriminant_Extension (N => 3) :=
           (D with N => 3, S2 => String_3);
      begin
         C432002_0.Do_Something (Nd); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is an variable");
         Report.Failed
           ("Aggregate of extension " &
            "with new discriminant: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Nd_Matched_Variable;

   -- Unsuccessful cases - value does not match value of corresponding
   --                      discriminant. Constraint_Error should be
   --                      raised.

   Nd_Unmatched_Aggregate : begin
      declare
         Nd : C432002_0.New_Discriminant_Extension (N => 20) :=
           (C432002_0.Discriminant'(L => 11, S1 => String_11) with
            N  => 20,
            S2 => String_20);
      begin
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension " &
            "with new discriminant: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Nd); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise is expected
   end Nd_Unmatched_Aggregate;

   Nd_Unmatched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 5) :=
           C432002_0.Discriminant'(L => 5, S1 => String_5);

         Nd : C432002_0.New_Discriminant_Extension (N => 20) :=
           (D with N => 20, S2 => String_20);
      begin
         Report.Comment ("Ancestor expression is an variable");
         Report.Failed
           ("Aggregate of extension " &
            "with new discriminant: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Nd); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise is expected
   end Nd_Unmatched_Variable;

   --------------------------------------------------------------------
   -- Extension constrains parent's discriminant to value of expression Parent
   -- is a discriminant extension
   --------------------------------------------------------------------

   -- Successful cases - value matches corresponding discriminant value

   Ce_Matched_Aggregate : begin
      declare
         Ce : C432002_0.Constrained_Extension_Extension :=
           (C432002_0.Discriminant'(L => 20, S1 => String_20) with
            N  => 20,
            S2 => String_20,
            S3 => String_5);
      begin
         C432002_0.Do_Something (Ce); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with discriminant constrained: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Ce_Matched_Aggregate;

   Ce_Matched_Variable : begin
      declare
         Nd : C432002_0.New_Discriminant_Extension (N => 20) :=
           C432002_0.New_Discriminant_Extension'
             (N => 20, S1 => String_20, S2 => String_20);

         Ce : C432002_0.Constrained_Extension_Extension :=
           (Nd with S3 => String_5);
      begin
         C432002_0.Do_Something (Ce); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is a variable");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with discriminant constrained: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Ce_Matched_Variable;

   -- Unsuccessful cases - value does not match value of corresponding
   --                      discriminant. Constraint_Error should be
   --                      raised.

   Ce_Unmatched_Aggregate : begin
      declare
         Ce : C432002_0.Constrained_Extension_Extension :=
           (C432002_0.New_Discriminant_Extension'
              (N => 11, S1 => String_11, S2 => String_11) with
            S3 => String_5);
      begin
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "Constraint_Error was not raised " &
            "with discriminant constrained: " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Ce); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise of Constraint_Error is expected
   end Ce_Unmatched_Aggregate;

   Ce_Unmatched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 8) :=
           C432002_0.Discriminant'(L => 8, S1 => String_8);

         Ce : C432002_0.Constrained_Extension_Extension :=
           (D with N => 8, S2 => String_8, S3 => String_5);
      begin
         Report.Comment ("Ancestor expression is a variable");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with discriminant constrained: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Ce); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise of Constraint_Error is expected
   end Ce_Unmatched_Variable;

   -----------------------------------------------------------------------
   -- Extension constrains parent's discriminant to equal new discriminant
   -- Parent is a discriminant extension
   -----------------------------------------------------------------------

   -- Successful cases - value matches corresponding discriminant value

   Ne_Matched_Aggregate : begin
      declare
         Ne : C432002_0.New_Extension_Extension (I => 8) :=
           (C432002_0.Discriminant'(L => 8, S1 => String_8) with
            I  => 8,
            S2 => String_8,
            S3 => String_8);
      begin
         C432002_0.Do_Something (Ne); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is an aggregate");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with new discriminant: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Ne_Matched_Aggregate;

   Ne_Matched_Variable : begin
      declare
         Nd : C432002_0.New_Discriminant_Extension (N => 3) :=
           C432002_0.New_Discriminant_Extension'
             (N => 3, S1 => String_3, S2 => String_3);

         Ne : C432002_0.New_Extension_Extension (I => 3) :=
           (Nd with I => 3, S3 => String_3);
      begin
         C432002_0.Do_Something (Ne); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Comment ("Ancestor expression is a variable");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with new discriminant: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Ne_Matched_Variable;

   -- Unsuccessful cases - value does not match value of corresponding
   --                      discriminant. Constraint_Error should be
   --                      raised.

   Ne_Unmatched_Aggregate : begin
      declare
         Ne : C432002_0.New_Extension_Extension (I => 8) :=
           (C432002_0.New_Discriminant_Extension'
              (C432002_0.Discriminant'(L => 11, S1 => String_11) with
               N  => 11,
               S2 => String_11) with
            I  => 8,
            S3 => String_8);
      begin
         Report.Comment ("Ancestor expression is an extension aggregate");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with new discriminant: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Ne); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise is expected
   end Ne_Unmatched_Aggregate;

   Ne_Unmatched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 5) :=
           C432002_0.Discriminant'(L => 5, S1 => String_5);

         Ne : C432002_0.New_Extension_Extension (I => 20) :=
           (D with I => 5, S2 => String_5, S3 => String_20);
      begin
         Report.Comment ("Ancestor expression is a variable");
         Report.Failed
           ("Aggregate of extension (of extension) " &
            "with new discriminant: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Ne); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise is expected
   end Ne_Unmatched_Variable;

   -----------------------------------------------------------------------
   -- Corresponding discriminant is two levels deeper than aggregate
   -----------------------------------------------------------------------

   -- Successful case - value matches corresponding discriminant value

   Tr_Matched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 10) :=
           C432002_0.Discriminant'(L => 10, S1 => String_10);

         Tr : C432002_0.Twice_Removed :=
           C432002_0.Twice_Removed'
             (D with S2 => String_20, S3 => String_3, S4 => String_8);
      -- N is constrained to a value in the derived_type_definition of
      -- Constrained_Discriminant_Extension. Its omission from the above
      -- record_component_association_list is allowed by 4.3.2(6).

      begin
         C432002_0.Do_Something (Tr); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Failed
           ("Aggregate of far-removed extension " &
            "with discriminant constrained: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Tr_Matched_Variable;

   -- Unsuccessful case - value does not match value of corresponding
   --                      discriminant. Constraint_Error should be
   --                      raised.

   Tr_Unmatched_Variable : begin
      declare
         D : C432002_0.Discriminant (L => 5) :=
           C432002_0.Discriminant'(L => 5, S1 => String_5);

         Tr : C432002_0.Twice_Removed :=
           C432002_0.Twice_Removed'
             (D with S2 => String_20, S3 => String_3, S4 => String_8);

      begin
         Report.Failed
           ("Aggregate of far-removed extension " &
            "with discriminant constrained: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Tr); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise is expected
   end Tr_Unmatched_Variable;

   ------------------------------------------------------------------------
   -- Parent has multiple discriminants. Discriminant in extension corresponds
   -- to both parental discriminants.
   ------------------------------------------------------------------------

   -- Successful case - value matches corresponding discriminant value

   Md_Matched_Variable : begin
      declare
         Md : C432002_0.Multiple_Discriminants (A => 10, B => 10) :=
           C432002_0.Multiple_Discriminants'
             (A => 10, B => 10, S1 => String_10, S2 => String_10);
         Mde : C432002_0.Multiple_Discriminant_Extension (C => 10) :=
           (Md with C => 10, S3 => String_10);

      begin
         C432002_0.Do_Something (Mde); -- success
      end;
   exception
      when Constraint_Error =>
         Report.Failed
           ("Aggregate of extension " &
            "of multiply-discriminated parent: " &
            "Constraint_Error was incorrectly raised " &
            "for value that matches corresponding " &
            "discriminant");
   end Md_Matched_Variable;

   -- Unsuccessful case - value does not match value of corresponding
   --                      discriminant. Constraint_Error should be
   --                      raised.

   Md_Unmatched_Variable : begin
      declare
         Md : C432002_0.Multiple_Discriminants (A => 10, B => 8) :=
           C432002_0.Multiple_Discriminants'
             (A => 10, B => 8, S1 => String_10, S2 => String_8);
         Mde : C432002_0.Multiple_Discriminant_Extension (C => 10) :=
           (Md with C => 10, S3 => String_10);

      begin
         Report.Failed
           ("Aggregate of extension " &
            "of multiply-discriminated parent: " &
            "Constraint_Error was not raised " &
            "for discriminant value that does not match " &
            "corresponding discriminant");
         C432002_0.Do_Something (Mde); -- disallow unused var optimization
      end;
   exception
      when Constraint_Error =>
         null; -- raise is expected
   end Md_Unmatched_Variable;

   Report.Result;

end C432002;

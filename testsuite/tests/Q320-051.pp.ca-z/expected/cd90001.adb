------------------------------------------------------------------- CD90001

with Report;
with Cd90001_0;

procedure Cd90001 is

   Eight_Na   : Boolean := False;
   Sixteen_Na : Boolean := False;

begin  -- Main test procedure.

   Report.Test
     ("CD90001",
      "Check that Unchecked_Conversion is supported " &
      "and is reversible in appropriate cases");
   Eight_Bit_Case :
   begin
      if Cd90001_0.User_Enums'Size /= Cd90001_0.Eight_Bits'Size then
         Report.Comment
           ("The sizes of the 8 bit types used in this test " &
            "do not match");
         Eight_Na := True;
      elsif Cd90001_0.User_Enums'Alignment /=
        Cd90001_0.Eight_Bits'Alignment
      then
         Report.Comment
           ("The alignments of the 8 bit types used in this " &
            "test do not match");
         Eight_Na := True;
      else
         Cd90001_0.Tc_Check_Case_1;
      end if;

   exception
      when Constraint_Error =>
         Report.Failed ("Constraint_Error raised in 8 bit case");
      when others =>
         Report.Failed ("Unexpected exception raised in 8 bit case");
   end Eight_Bit_Case;

   Sixteen_Bit_Case :
   begin
      if Cd90001_0.Signed_16'Size /= Cd90001_0.Bits_16'Size then
         Report.Comment
           ("The sizes of the 16 bit types used in this test " &
            "do not match");
         Sixteen_Na := True;
      elsif Cd90001_0.Signed_16'Alignment /= Cd90001_0.Bits_16'Alignment then
         Report.Comment
           ("The alignments of the 16 bit types used in this " &
            "test do not match");
         Sixteen_Na := True;
      else
         Cd90001_0.Tc_Check_Case_2;
      end if;

   exception
      when Constraint_Error =>
         Report.Failed ("Constraint_Error raised in 16 bit case");
      when others =>
         Report.Failed ("Unexpected exception raised in 16 bit case");
   end Sixteen_Bit_Case;

   if Eight_Na and Sixteen_Na then
      Report.Not_Applicable ("No cases in this test apply");
   end if;

   Report.Result;

end Cd90001;

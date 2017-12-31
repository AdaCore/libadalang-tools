     --==================================================================--

with C460005_0; use C460005_0;

with Report;
procedure C460005 is

   Tag_Type_Init   : constant Tag_Type   := (C1 => 0);
   Dtag_Type_Init  : constant Dtag_Type  := (Tag_Type_Init with "Hello");
   Ddtag_Type_Init : constant Ddtag_Type := (Dtag_Type_Init with "World");

   Tag_Type_Value   : constant Tag_Type   := (C1 => 25);
   Dtag_Type_Value  : constant Dtag_Type  := (Tag_Type_Value with "Earth");
   Ddtag_Type_Value : constant Ddtag_Type := (Dtag_Type_Value with "Orbit");

   Tag_Type_Res   : constant Tag_Type   := (C1 => 25);
   Dtag_Type_Res  : constant Dtag_Type  := (Tag_Type_Res with "Hello");
   Ddtag_Type_Res : constant Ddtag_Type := (Dtag_Type_Res with "World");

begin

   Report.Test
     ("C460005",
      "Check that, for a view conversion of a tagged " &
      "type that is the left side of an assignment statement, " &
      "the assignment assigns to the corresponding part of the " &
      "object denoted by the operand");

   declare
      procedure Cw_Proc (P : Tag_Type'Class) is
         Operand : Tag_Type'Class := P;
      begin
         Tag_Type (Operand) := Tag_Type_Value;

         if (Operand /= Tag_Type'Class (Tag_Type_Value)) then
            Report.Failed ("Operand has wrong value: #01");
         end if;

      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised: #01");
         when others =>
            Report.Failed ("Unexpected exception: #01");
      end Cw_Proc;

   begin
      Cw_Proc (Tag_Type_Init);
   end;

   ----------------------------------------------------------------------

   declare
      procedure Cw_Proc (P : Tag_Type'Class) is
         Operand : Tag_Type'Class := P;
      begin
         Dtag_Type (Operand) := Dtag_Type_Value;
         Report.Failed ("Constraint_Error not raised: #02");

      exception
         when Constraint_Error =>
            null;              -- expected exception
         when others =>
            Report.Failed ("Unexpected exception: #02");
      end Cw_Proc;

   begin
      Cw_Proc (Tag_Type_Init);
   end;

   ----------------------------------------------------------------------

   declare
      procedure Cw_Proc (P : Tag_Type'Class) is
         Operand : Tag_Type'Class := P;
      begin
         Ddtag_Type (Operand) := Ddtag_Type_Value;
         Report.Failed ("Constraint_Error not raised: #03");

      exception
         when Constraint_Error =>
            null;              -- expected exception
         when others =>
            Report.Failed ("Unexpected exception: #03");
      end Cw_Proc;

   begin
      Cw_Proc (Tag_Type_Init);
   end;

   ----------------------------------------------------------------------

   declare
      procedure Cw_Proc (P : Tag_Type'Class) is
         Operand : Tag_Type'Class := P;
      begin
         Tag_Type (Operand) := Tag_Type_Value;

         if Operand not in Dtag_Type then
            Report.Failed ("Operand has wrong tag: #04");
         elsif (Operand /= Tag_Type'Class (Dtag_Type_Res))
         then                                              -- Check to make
            Report.Failed ("Operand has wrong value: #04"); -- sure that C2 was
         end if;                                           -- not modified.

      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised: #04");
         when others =>
            Report.Failed ("Unexpected exception: #04");
      end Cw_Proc;

   begin
      Cw_Proc (Dtag_Type_Init);
   end;

   ----------------------------------------------------------------------

   declare
      procedure Cw_Proc (P : Tag_Type'Class) is
         Operand : Tag_Type'Class := P;
      begin
         Tag_Type (Operand) := Tag_Type_Value;

         if Operand not in Ddtag_Type then
            Report.Failed ("Operand has wrong tag: #05");
         elsif (Operand /= Tag_Type'Class (Ddtag_Type_Res))
         then                                              -- Check to make
            Report.Failed ("Operand has wrong value: #05"); -- sure that C2, C3
         end if;                                           -- were not changed.

      exception
         when Constraint_Error =>
            Report.Failed ("Constraint_Error raised: #05");
         when others =>
            Report.Failed ("Unexpected exception: #05");
      end Cw_Proc;

   begin
      Cw_Proc (Ddtag_Type_Init);
   end;

   Report.Result;

end C460005;

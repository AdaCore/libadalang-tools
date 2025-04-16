with Ada.Text_IO;                 use Ada.Text_IO;
with My_File.TGen_Support;        use My_File.TGen_Support;
with TGen.TGen_Support;           use TGen.TGen_Support;
with TGen.JSON;

procedure Example_Gen is

   String_Inst : constant String (1 .. 10) := (others => <>);

   Unit_JSON : TGen.JSON.JSON_Value := TGen.JSON.Create_Object;

begin

   my_file_is_null_df697b40227a3e08_Dump_TC
     (TGen_Marshalling_S         => String_Inst,
      TGen_Unit_JSON => Unit_JSON,
      TGen_Test_Origin    => "First write");

   my_file_is_null_df697b40227a3e08_Dump_TC
     (TGen_Marshalling_S         => String_Inst,
      TGen_Unit_JSON => Unit_JSON,
      TGen_Test_Origin    => "First write");

   if TGen_Marshalling_Standard_String_Input
     (TGen_Marshalling_Standard_String_Output (String_Inst)) /= String_Inst
   then
      raise Program_Error;
   end if;

   Put_Line (Unit_JSON.Write (Compact => False));

end;

with Ada.Text_IO;                 use Ada.Text_IO;
with My_File;                     use My_File;
with My_File.TGen_Support;        use My_File.TGen_Support;
with TGen.JSON;
with TGen.TGen_Support;           use TGen.TGen_Support;

procedure Example_Gen is

   R_Inst : R :=
     (F1 => 1,
      F2 => 100,
      F3 => 2**8,
      F4 => True,
      F5 => 5.0,
      F6 => 7.0,
      F7 => 3.14,
      F8 => (1,2,3,4,5,6,7,8,9,10),
      F9 => 'C',
      G1 => 0.003,
      G2 => 10_000_000.0,
      G4 =>
        (K => Ellipse,
         Name_Size => 10,
         Name => "my_Ellipse",
         X => 1,
         Y => 1,
         Radius => 1,
         Radius_2 => 10),
      G5 => (K => Point, Name_Size => 3, Name => "Ptr", X => -1, Y => -5),
      G6 => (L => 0, others => <>));

   T2_Inst : constant T2 := 20;

   String_Inst : constant String := "foo bar";

   Matrix_Inst : constant Matrix :=
     (1 => ('a' => True, 'b' => False), 2 => ('a' => False, 'b' => True));

   R2_Inst : constant R2 := (F1 => True, F2 => False);

   Shape_Inst : constant Shape :=
     (K => Line,
      Name_Size => 8,
      Name => "My_Line ",
      X => 0,
      Y =>0,
      X_2 => 1,
      Y_2 => 2);

   Shape_Arr_Inst : constant Shape_Array (1 .. 2) :=
     (1 => Shape_Inst,
      others =>
        (K => Point,
         Name_Size => 10,
         Name => "Null_Shape",
         X => 10,
         Y => 7));

   T_Gen_Inst : constant T_Gen := 9;

   T_Null_Inst : T_Null;

   Unit_JSON : TGen.JSON.JSON_Value := TGen.JSON.Create_Object;

begin

   My_File_Test_Proc_0e31b625e8e24f10_Dump_TC
     (TGen_Marshalling_A         => String_Inst,
      TGen_Marshalling_D         => Shape_Inst,
      TGen_Marshalling_M         => Matrix_Inst,
      TGen_Marshalling_V         => Shape_Arr_Inst,
      TGen_Marshalling_X         => R_Inst,
      TGen_Marshalling_Y         => T2_Inst,
      TGen_Marshalling_Z         => R2_Inst,
      TGen_Unit_JSON => Unit_JSON,
      TGen_Test_Origin    => "First write");

   My_File_Test_Proc_0e31b625e8e24f10_Dump_TC
     (TGen_Marshalling_A         => String_Inst,
      TGen_Marshalling_D         => Shape_Inst,
      TGen_Marshalling_M         => Matrix_Inst,
      TGen_Marshalling_V         => Shape_Arr_Inst,
      TGen_Marshalling_X         => R_Inst,
      TGen_Marshalling_Y         => T2_Inst,
      TGen_Marshalling_Z         => R2_Inst,
      TGen_Unit_JSON => Unit_JSON,
      TGen_Test_Origin    => "Second write");

   my_file_actual_ident_5e1d01120f468a24_Dump_TC (T_Gen_Inst, Unit_JSON, "First write");
   my_file_actual_ident_5e1d01120f468a24_Dump_TC (T_Gen_Inst, Unit_JSON, "Second write");
   my_file_use_null_rec_d9e626ae7e5c9f3e_Dump_TC (T_Null_Inst, Unit_JSON, "First Write");
   my_file_use_null_rec_d9e626ae7e5c9f3e_Dump_TC (T_Null_Inst, Unit_JSON, "Second Write");

   Put_Line (Unit_JSON.Write (Compact => False));

end;

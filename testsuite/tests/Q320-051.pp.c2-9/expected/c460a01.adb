     --==================================================================--

with F460a00;
with C460a01_0;
with C460a01_1;
with C460a01_2;

with Report;
procedure C460a01 is
begin -- C460A01.                                              -- [ Level = 1 ]

   Report.Test
     ("C460A01",
      "Run-time accessibility checks: instance " &
      "bodies. Operand type of access type conversion is " &
      "passed as actual to instance");

   Subtest1 : declare                                                     -- [ Level = 2 ]
      type Acctag_L2 is access all F460a00.Tagged_Type;
      Operand : Acctag_L2 := new F460a00.Tagged_Type;

      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST1.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C460A01_0 should NOT result in any exceptions.

         package Pack_Ok is new C460a01_0 (F460a00.Tagged_Type, Acctag_L2);
         Target : Pack_Ok.Target_Type;
      begin
         -- The accessibility level of Pack_OK.Target_Type will always be at
         -- least as deep as the operand type passed as an actual. Thus, a
         -- call to Pack_OK.Convert does not propagate an exception:

         Target := Pack_Ok.Convert (Operand);

         -- Avoid optimization (dead variable removal of Target):
         if not Report.Equal (Target.C, Target.C) then      -- Always false.
            Report.Failed ("Unexpected error in SUBTEST #1");
         end if;

         Result := F460a00.Ok;                              -- Expected result.
      exception
         when Program_Error =>
            Result := F460a00.Pe_Exception;
         when others =>
            Result := F460a00.Others_Exception;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Ok, "SUBTEST #1");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #1: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #1: Unexpected exception raised");
   end Subtest1;

   Subtest2 : declare                                                     -- [ Level = 2 ]
      type Acctag_L2 is access all F460a00.Tagged_Type;
      Operand : Acctag_L2 := new F460a00.Tagged_Type;

      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST2.

      declare                                                  -- [ Level = 3 ]

         type Acctag_L3 is access all F460a00.Tagged_Type;
         Target : Acctag_L3;

         -- The instantiation of C460A01_1 should NOT result in any exceptions.

         package Pack_Ok is new C460a01_1
           (Designated_Type => F460a00.Tagged_Type,
            Operand_Type    => Acctag_L2,
            Target_Type     => Acctag_L3);
      begin
         -- The accessibility level of the actual passed as the operand type
         -- in Pack_OK is 2. The accessibility level of the actual passed as
         -- the target type is 3. Therefore, the access type conversion in
         -- Pack_OK.Convert does not raise an exception when the subprogram is
         -- called. If an exception is (incorrectly) raised, it is propagated
         -- to the innermost enclosing master:

         Target := Pack_Ok.Convert (Operand);

         -- Avoid optimization (dead variable removal of Target):
         if not Report.Equal (Target.C, Target.C) then      -- Always false.
            Report.Failed ("Unexpected error in SUBTEST #2");
         end if;

         Result := F460a00.Ok;                              -- Expected result.
      exception
         when Program_Error =>
            Result := F460a00.Pe_Exception;
         when others =>
            Result := F460a00.Others_Exception;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Ok, "SUBTEST #2");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #2: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #2: Unexpected exception raised");
   end Subtest2;

   Subtest3 : declare                                                     -- [ Level = 2 ]
      type Acctag_L2 is access all F460a00.Tagged_Type;
      Target : Acctag_L2;

      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST3.

      declare                                                  -- [ Level = 3 ]

         type Acctag_L3 is access all F460a00.Tagged_Type;
         Operand : Acctag_L3 := new F460a00.Tagged_Type;

         -- The instantiation of C460A01_1 should NOT result in any exceptions.

         package Pack_Pe is new C460a01_1
           (Designated_Type => F460a00.Tagged_Type,
            Operand_Type    => Acctag_L3,
            Target_Type     => Acctag_L2);
      begin
         -- The accessibility level of the actual passed as the operand type
         -- in Pack_PE is 3. The accessibility level of the actual passed as
         -- the target type is 2. Therefore, the access type conversion in
         -- Pack_PE.Convert raises Program_Error when the subprogram is called.
         -- The exception is propagated to the innermost enclosing master:

         Target := Pack_Pe.Convert (Operand);

         -- Avoid optimization (dead variable removal of Target):
         if not Report.Equal (Target.C, Target.C) then      -- Always false.
            Report.Failed ("Unexpected error in SUBTEST #3");
         end if;

         Result := F460a00.Ok;
      exception
         when Program_Error =>
            Result := F460a00.Pe_Exception;
         -- Expected result.
         when others =>
            Result := F460a00.Others_Exception;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Pe_Exception, "SUBTEST #3");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #3: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #3: Unexpected exception raised");
   end Subtest3;

   Subtest4 : declare                                                     -- [ Level = 2 ]
      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST4.

      declare                                                  -- [ Level = 3 ]

         Ttype   : F460a00.Tagged_Type;
         Operand : F460a00.Acctagclass_L0 := new F460a00.Tagged_Type'(Ttype);

         -- The instantiation of C460A01_2 should NOT result in any exceptions.

         package Pack_Ok is new C460a01_2
           (F460a00.Tagged_Type'Class,
            F460a00.Acctagclass_L0);
      begin
         -- The accessibility level of the actual passed as the operand
         -- type in Pack_OK is 0. The accessibility level of the target
         -- type (F460A00.AccTag_L0) is also 0. Therefore, the access type
         -- conversion in Pack_OK.Proc does not raise an exception when the
         -- subprogram is called. If an exception is (incorrectly) raised,
         -- it is handled within the subprogram:

         Pack_Ok.Proc (Operand, Result);
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Ok, "SUBTEST #4");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #4: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #4: Unexpected exception raised");
   end Subtest4;

   Subtest5 : declare                                                     -- [ Level = 2 ]
      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST5.

      declare                                                  -- [ Level = 3 ]

         type Accdertag_L3 is access all F460a00.Derived_Tagged_Type;
         Operand : Accdertag_L3 := new F460a00.Derived_Tagged_Type;

         -- The instantiation of C460A01_2 should NOT result in any exceptions.

         package Pack_Pe is new C460a01_2
           (F460a00.Derived_Tagged_Type,
            Accdertag_L3);
      begin
         -- The accessibility level of the actual passed as the operand
         -- type in Pack_PE is 3. The accessibility level of the target type
         -- (F460A00.AccTag_L0) is 0. Therefore, the access type conversion in
         -- Pack_PE.Proc raises Program_Error when the subprogram is called.
         -- The exception is handled within the subprogram:

         Pack_Pe.Proc (Operand, Result);
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Pe_Exception, "SUBTEST #5");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #5: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #5: Unexpected exception raised");
   end Subtest5;

   Report.Result;

end C460a01;

     --==================================================================--

with F460a00;
with C460a02_0;
with C460a02_1;
with C460a02_2;

with Report;
procedure C460a02 is
begin -- C460A02.                                              -- [ Level = 1 ]

   Report.Test
     ("C460A02",
      "Run-time accessibility checks: instance " &
      "bodies. Operand type of access type conversion is " &
      "declared inside instance or is anonymous");

   Subtest1 : declare                                                     -- [ Level = 2 ]
      type Acctag_L2 is access all F460a00.Tagged_Type;
      Ptag_L2    : Acctag_L2 := new F460a00.Tagged_Type;
      Operand_L2 : F460a00.Composite_Type (Ptag_L2);

      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST1.

      begin                                                    -- [ Level = 3 ]
         declare                                               -- [ Level = 4 ]
            -- The accessibility level of the actual passed as the target type
            -- in Pack_OK is 2. The accessibility level of the composite actual
            -- (and thus, the level of the anonymous type of the access
            -- discriminant, which is the same as that of the containing
            -- object) is also 2. Therefore, the access type conversion in
            -- Pack_OK does not raise an exception upon instantiation:

            package Pack_Ok is new C460a02_0
              (Target_Type => Acctag_L2,
               Fobj        => Operand_L2);
         begin
            Result := F460a00.Ok;                           -- Expected result.
         end;
      exception
         when Program_Error =>
            Result := F460a00.Pe_Exception;
         when others =>
            Result := F460a00.Others_Exception;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Ok, "SUBTEST #1");

   end Subtest1;

   Subtest2 : declare                                                     -- [ Level = 2 ]
      type Acctag_L2 is access all F460a00.Tagged_Type;
      Ptag_L2 : Acctag_L2 := new F460a00.Tagged_Type;

      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST2.

      declare                                                  -- [ Level = 3 ]
         Operand_L3 : F460a00.Composite_Type (Ptag_L2);
      begin
         declare                                               -- [ Level = 4 ]
            -- The accessibility level of the actual passed as the target type
            -- in Pack_PE is 2. The accessibility level of the composite actual
            -- (and thus, the level of the anonymous type of the access
            -- discriminant, which is the same as that of the containing
            -- object) is 3. Therefore, the access type conversion in Pack_PE
            -- propagates Program_Error upon instantiation:

            package Pack_Pe is new C460a02_0 (Acctag_L2, Operand_L3);
         begin
            Result := F460a00.Ok;
         end;
      exception
         when Program_Error =>
            Result := F460a00.Pe_Exception;
         -- Expected result.
         when others =>
            Result := F460a00.Others_Exception;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Pe_Exception, "SUBTEST #2");

   end Subtest2;

   Subtest3 : declare                                                     -- [ Level = 2 ]
      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST3.

      declare                                                  -- [ Level = 3 ]
         type Accarr_L3 is access all F460a00.Array_Type;
         Target : Accarr_L3;

         -- The accessibility level of the actual passed as the target type
         -- in Pack_OK is 3. The accessibility level of the operand type is
         -- that of the instance, which is also 3. Therefore, the access type
         -- conversion in Pack_OK does not raise an exception upon
         -- instantiation. If an exception is (incorrectly) raised, it is
         -- handled within the instance:

         package Pack_Ok is new C460a02_1
           (Designated_Type => F460a00.Array_Type,
            Target_Type     => Accarr_L3,
            Fobj            => Target,
            Fres            => Result);
      begin
         null;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Ok, "SUBTEST #3");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #3: Program_Error incorrectly propagated");
      when others =>
         Report.Failed ("SUBTEST #3: Unexpected exception propagated");
   end Subtest3;

   Subtest4 : declare                                                     -- [ Level = 2 ]
      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST4.

      declare                                                  -- [ Level = 3 ]
         Target : F460a00.Accarr_L0;

         -- The accessibility level of the actual passed as the target type
         -- in Pack_PE is 0. The accessibility level of the operand type is
         -- that of the instance, which is 3. Therefore, the access type
         -- conversion in Pack_PE raises Program_Error upon instantiation.
         -- The exception is handled within the instance:

         package Pack_Pe is new C460a02_1
           (Designated_Type => F460a00.Array_Type,
            Target_Type     => F460a00.Accarr_L0,
            Fobj            => Target,
            Fres            => Result);
      begin
         null;
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Pe_Exception, "SUBTEST #4");

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
         -- The instantiation of C460A02_2 should NOT result in any
         -- exceptions.

         procedure Proc is new C460a02_2
           (F460a00.Tagged_Type,
            F460a00.Acctag_L0);
      begin
         -- The accessibility level of the actual passed to Proc is 0. The
         -- accessibility level of the actual passed as the target type is
         -- also 0. Therefore, the access type conversion in Proc does not
         -- raise an exception when the subprogram is called. If an exception
         -- is (incorrectly) raised, it is handled within the subprogram:

         Proc (F460a00.Ptagclass_L0, Result);
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Ok, "SUBTEST #5");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #5: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #5: Unexpected exception raised");
   end Subtest5;

   Subtest6 : declare                                                     -- [ Level = 2 ]
      Result : F460a00.Tc_Result_Kind := F460a00.Un_Init;
   begin -- SUBTEST6.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C460A02_2 should NOT result in any
         -- exceptions.

         procedure Proc is new C460a02_2
           (F460a00.Tagged_Type,
            F460a00.Acctag_L0);
      begin
         -- In the call to (instantiated) procedure Proc, the first actual
         -- parameter is an allocator. Its accessibility level is that of
         -- the level of execution of Proc, which is 3. The accessibility
         -- level of the actual passed as the target type is 0.  Therefore,
         -- the access type conversion in Proc raises Program_Error when the
         -- subprogram is called. The exception is handled within the
         -- subprogram:

         Proc (new F460a00.Tagged_Type, Result);
      end;

      F460a00.Tc_Check_Results (Result, F460a00.Pe_Exception, "SUBTEST #6");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #6: Program_Error incorrectly raised");
      when others =>
         Report.Failed ("SUBTEST #6: Unexpected exception raised");
   end Subtest6;

   Report.Result;

end C460a02;

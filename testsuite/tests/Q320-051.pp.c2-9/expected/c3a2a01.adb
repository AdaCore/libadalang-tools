     --==================================================================--

with F3a2a00;
with C3a2a01_0;
with C3a2a01_1;
with C3a2a01_2;

with Report;
procedure C3a2a01 is
begin -- C3A2A01.                                              -- [ Level = 1 ]

   Report.Test
     ("C3A2A01",
      "Run-time accessibility checks: instance " &
      "bodies. Type of X'Access is passed as actual to instance");

   Subtest1 :
   declare                                                     -- [ Level = 2 ]
      Result : F3a2a00.Tc_Result_Kind;
   begin -- SUBTEST1.

      declare                                                  -- [ Level = 3 ]
         type Accarr_L3 is access all F3a2a00.Array_Type;
      begin
         declare                                               -- [ Level = 4 ]
            -- The accessibility level of Pack.X is that of the instantiation
            -- (4). The accessibility level of the actual access type used
            -- to instantiate Pack is 3. Therefore, the X'Access in Pack
            -- propagates Program_Error when the instance body is elaborated:

            package Pack is new C3a2a01_0 (F3a2a00.Array_Type, Accarr_L3);
         begin
            Result := F3a2a00.Ok;
         end;
      exception
         when Program_Error =>
            Result := F3a2a00.P_E;       -- Expected result.
         when others =>
            Result := F3a2a00.O_E;
      end;

      F3a2a00.Tc_Display_Results (Result, F3a2a00.P_E, "SUBTEST #1");

   end Subtest1;

   Subtest2 :
   declare                                                     -- [ Level = 2 ]
      Result : F3a2a00.Tc_Result_Kind;
   begin -- SUBTEST2.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C3A2A01_1 should NOT result in any exceptions.

         type Acctag_L3 is access all F3a2a00.Tagged_Type;

         package Pack_Ok is new C3a2a01_1 (F3a2a00.Tagged_Type, Acctag_L3,
            F3a2a00.X_L0);
      begin
         -- The accessibility level of the actual object used to instantiate
         -- Pack_OK is 0. The accessibility level of the actual access type
         -- used to instantiate Pack_OK is 3. Therefore, the FObj'Access in
         -- Pack_OK.Handle does not raise an exception when the subprogram is
         -- called. If an exception is (incorrectly) raised, however, it is
         -- handled within the subprogram:

         Pack_Ok.Handle (Result);
      end;

      F3a2a00.Tc_Display_Results (Result, F3a2a00.Ok, "SUBTEST #2");

   exception
      when Program_Error =>
         Report.Failed
           ("SUBTEST #2: Program_Error incorrectly raised " &
            "during instantiation of generic");
      when others =>
         Report.Failed
           ("SUBTEST #2: Unexpected exception raised " &
            "during instantiation of generic");
   end Subtest2;

   Subtest3 :
   declare                                                     -- [ Level = 2 ]
      Result : F3a2a00.Tc_Result_Kind;
   begin -- SUBTEST3.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C3A2A01_1 should NOT result in any exceptions.

         X_L3 : F3a2a00.Tagged_Type;

         package Pack_Pe is new C3a2a01_1 (F3a2a00.Tagged_Type,
            F3a2a00.Acctag_L0, X_L3);
      begin
         -- The accessibility level of the actual object used to instantiate
         -- Pack_PE is 3. The accessibility level of the actual access type
         -- used to instantiate Pack_PE is 0. Therefore, the FObj'Access in
         -- Pack_OK.Handle raises Program_Error when the subprogram is called.
         -- The exception is handled within the subprogram:

         Pack_Pe.Handle (Result);
      end;

      F3a2a00.Tc_Display_Results (Result, F3a2a00.P_E, "SUBTEST #3");

   exception
      when Program_Error =>
         Report.Failed
           ("SUBTEST #3: Program_Error incorrectly raised " &
            "during instantiation of generic");
      when others =>
         Report.Failed
           ("SUBTEST #3: Unexpected exception raised " &
            "during instantiation of generic");
   end Subtest3;

   Subtest4 :
   declare                                                     -- [ Level = 2 ]
      Result1 : F3a2a00.Tc_Result_Kind;
      Result2 : F3a2a00.Tc_Result_Kind;
   begin -- SUBTEST4.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C3A2A01_2 should NOT result in any exceptions.

         X_L3 : aliased F3a2a00.Array_Type;
         type Accarr_L3 is access all F3a2a00.Array_Type;

         procedure Proc is new C3a2a01_2 (F3a2a00.Array_Type, Accarr_L3);
      begin
         -- The accessibility level of Proc.P.all is that of the corresponding
         -- actual during the call (in this case 3). The accessibility level
         -- of the access type used to instantiate Proc is also 3. Therefore,
         -- the P.all'Access in Proc does not raise an exception when the
         -- subprogram is called. If an exception is (incorrectly) raised,
         -- however, it is handled within the subprogram:

         Proc (X_L3'Access, Result1);

         F3a2a00.Tc_Display_Results
           (Result1, F3a2a00.Ok, "SUBTEST #4: same levels");

         declare                                               -- [ Level = 4 ]
            X_L4 : aliased F3a2a00.Array_Type;
         begin
            -- Within this block, the accessibility level of the actual
            -- corresponding to Proc.P.all is 4. Therefore, the P.all'Access
            -- in Proc raises Program_Error when the subprogram is called. The
            -- exception is handled within the subprogram:

            Proc (X_L4'Access, Result2);

            F3a2a00.Tc_Display_Results
              (Result2, F3a2a00.P_E, "SUBTEST #4: object at deeper level");
         end;

      end;

   exception
      when Program_Error =>
         Report.Failed
           ("SUBTEST #4: Program_Error incorrectly raised " &
            "during instantiation of generic");
      when others =>
         Report.Failed
           ("SUBTEST #4: Unexpected exception raised " &
            "during instantiation of generic");
   end Subtest4;

   Report.Result;

end C3a2a01;

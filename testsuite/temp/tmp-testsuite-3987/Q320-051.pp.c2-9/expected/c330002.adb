--==================================================================--

with Report;
with C330002_0; use C330002_0;

procedure C330002 is

begin
   Report.Test
     ("C330002",
      "Check that if a subtype indication of a " &
      "variable object defines an indefinite subtype, then " &
      "there is an initialization expression.  Check that " &
      "the object remains so constrained throughout its " &
      "lifetime.  Check that Constraint_Error is raised " &
      "if an attempt is made to change bounds as well as " &
      "discriminants of the objects of the indefinite " &
      "subtypes.  Check for cases of tagged record and generic " &
      "formal types");

   Tagobj_Block : declare
      Tobj_Byagg : Tag_Type := (5, "Hello");    -- Initial assignment is
      -- aggregate.
      Tobj_Byobj : Tag_Type := Tobj_Byagg;      -- Initial assignment is
      -- an object.
      Tobj_Byfunc : Tag_Type := Tag_Value;       -- Initial assignment is
      -- function return value.
      Ren_Obj : Tag_Type renames Tobj_Byagg;

   begin

      begin
         if (Tobj_Byagg.Disc /= 5) or (Tobj_Byagg.S /= "Hello") then
            Report.Failed ("Wrong initial values for TObj_ByAgg");
         end if;

         Tobj_Byagg := (2, "Hi");                -- C_E, can't change the
         -- value of the discriminant.

         Avoid_Optimization_And_Fail (Tobj_Byagg, "Subtest 1");

      exception
         when Constraint_Error =>
            null;          -- Exception is expected.
         when others =>
            Report.Failed ("Unexpected exception - Subtest 1");
      end;

      begin
         Assign_Tag (Ren_Obj);                   -- C_E, can't change the
         -- value of the discriminant.

         Avoid_Optimization_And_Fail (Ren_Obj, "Subtest 2");

      exception
         when Constraint_Error =>
            null;          -- Exception is expected.
         when others =>
            Report.Failed ("Unexpected exception - Subtest 2");
      end;

      begin
         if (Tobj_Byobj.Disc /= 5) or (Tobj_Byobj.S /= "Hello") then
            Report.Failed ("Wrong initial values for TObj_ByObj");
         end if;

         Tobj_Byobj := (3, "Bye");               -- C_E, can't change the
         -- value of the discriminant.

         Avoid_Optimization_And_Fail (Tobj_Byobj, "Subtest 3");

      exception
         when Constraint_Error =>
            null;          -- Exception is expected.
         when others =>
            Report.Failed ("Unexpected exception - Subtest 3");
      end;

      begin
         if (Tobj_Byfunc.Disc /= 4) or (Tobj_Byfunc.S /= "ACVC") then
            Report.Failed ("Wrong initial values for TObj_ByFunc");
         end if;

         Tobj_Byfunc := (5, "Aloha");            -- C_E, can't change the
         -- value of the discriminant.

         Avoid_Optimization_And_Fail (Tobj_Byfunc, "Subtest 4");

      exception
         when Constraint_Error =>
            null;          -- Exception is expected.
         when others =>
            Report.Failed ("Unexpected exception - Subtest 4");
      end;

   end Tagobj_Block;

   Arrobj_Block : declare
      Arr_Const : constant Array_Type := (9, 7, 6, 8);
      Arr_Byagg : Array_Type                  -- Initial assignment is
      := (10, 11, 12);               -- aggregate.
      Arr_Byfunc : Array_Type                  -- Initial assignment is
      := Array_Value;                -- function return value.
      Arr_Byobj : Array_Type                  -- Initial assignment is
      := Arr_Byagg;                  -- object.

      Arr_Obj : array (Positive range <>) of Integer := (1, 2, 3, 4, 5);
   begin

      begin
         if (Arr_Const'First /= 1) or (Arr_Const'Last /= 4) then
            Report.Failed ("Wrong bounds for Arr_Const");
         end if;

         if (Arr_Byagg'First /= 1) or (Arr_Byagg'Last /= 3) then
            Report.Failed ("Wrong bounds for Arr_ByAgg");
         end if;

         if (Arr_Byfunc'First /= 1) or (Arr_Byfunc'Last /= 2) then
            Report.Failed ("Wrong bounds for Arr_ByFunc");
         end if;

         if (Arr_Byobj'First /= 1) or (Arr_Byobj'Last /= 3) then
            Report.Failed ("Wrong bounds for Arr_ByObj");
         end if;

         Assign_Array (Arr_Byobj);               -- C_E, Arr_ByObj bounds are
         -- 1..3.

         Report.Failed ("Constraint_Error not raised - Subtest 5");

      exception
         when Constraint_Error =>
            null;        -- Exception is expected.
         when others =>
            Report.Failed ("Unexpected exception - Subtest 5");
      end;

      begin
         if (Arr_Obj'First /= 1) or (Arr_Obj'Last /= 5) then
            Report.Failed ("Wrong bounds for Arr_Obj");
         end if;

         for I in 0 .. 5 loop
            Arr_Obj (I + 1) := I + 5;             -- C_E, Arr_Obj bounds are
         end loop;                                -- 1..5.

         Report.Failed ("Constraint_Error not raised - Subtest 6");

      exception
         when Constraint_Error =>
            null;        -- Exception is expected.
         when others =>
            Report.Failed ("Unexpected exception - Subtest 6");
      end;

   end Arrobj_Block;

   Genericobj_Block : declare
      type Rec (Disc : Small_Num) is record
         S : Small_Num := Disc;
      end record;

      Rec_Obj : Rec := (2, 2);
      package Igen is new Gen (Rec, Rec_Obj);

   begin
      Igen.Gen_Obj := (3, 3);                    -- C_E, can't change the
      -- value of the discriminant.

      Report.Failed ("Constraint_Error not raised - Subtest 7");

      -- Next line prevents dead assignment.
      Report.Comment ("Disc is" & Integer'Image (Igen.Gen_Obj.Disc));

   exception
      when Constraint_Error =>
         null;             -- Exception is expected.
      when others =>
         Report.Failed ("Unexpected exception - Subtest 7");

   end Genericobj_Block;

   Report.Result;

end C330002;

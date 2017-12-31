with Report;
with C413004q;
procedure C413004 is
begin
   Report.Test
     ("C413004",
      "Check that for the prefixed view L.R, if L " &
      "represents an object or value of an access type " &
      "that designates a tagged type T, " &
      "that R may represent a subprogram with a first " &
      "access parameter that designates the type T or " &
      "a class-wide type covered by T that is declared " &
      "immediately in the declarative region of " & "an ancestor of T.");

   -- Verify that the primitive operation in the ancestor is properly called.

   declare
      Q1_Ptr : access C413004q.Tq := new C413004q.Tq;
      Q2_Ptr : access C413004q.Tq := new C413004q.Tq;
   begin
      -- -------------------------------------------------------------------
      -- call on inherited procedure (with no parameters)

      Q1_Ptr.Base_Proc;
      C413004q.Base_Proc (Q2_Ptr);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (inherited procedure -1-)");
      end if;

      -- call on inherited function (with no parameters)
      if Q1_Ptr.Base_Func /= C413004q.Base_Func (Q1_Ptr) then
         Report.Failed ("Wrong value (inherited function -2-)");
      end if;

      -- call on inherited procedure (with parameters)
      Q1_Ptr.Base_Proc (Value => 123);
      if Q1_Ptr.Data /= 123 then
         Report.Failed ("Wrong value (inherited procedure -3-)");
      end if;

      --  call on inherited function (with parameters)
      if Q1_Ptr.Base_Func (Value => 1_234) /=
        C413004q.Base_Func (Q1_Ptr, 1_234) then
         Report.Failed ("Wrong value (inherited function -4-)");
      end if;
   end;

   -- -----------------------------------------------------------------
   --  Verify that the redefined primitive operation is properly called.

   declare
      Q1_Ptr : access C413004q.Tq := new C413004q.Tq;
      Q2_Ptr : access C413004q.Tq := new C413004q.Tq;
   begin
      --  call on primitive procedure (with no parameters)

      Q1_Ptr.Prim_Proc;
      C413004q.Prim_Proc (Q2_Ptr);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (primitive procedure -1-)");
      end if;

      --  call on primitive function (with no parameters)
      if Q1_Ptr.Prim_Func /= C413004q.Prim_Func (Q1_Ptr) then
         Report.Failed ("Wrong value (primitive function -2-)");
      end if;

      --  call on primitive procedure (with parameters)
      Q1_Ptr.Prim_Proc (Value => 123);
      C413004q.Prim_Proc (Q2_Ptr, Value => 123);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (primitive procedure -3-)");
      end if;

      --  call on primitive function (with parameters)
      if Q1_Ptr.Prim_Func (Value => 123) /=
        C413004q.Prim_Func (Q1_Ptr, Value => 123) then
         Report.Failed ("Wrong value (primitive function -4-)");
      end if;
   end;

   -- -------------------------------------------------------------------
   --  Verify dispatching calls.

   declare
      Q1_Ptr  : access C413004q.Tq       := new C413004q.Tq;
      Q2_Ptr  : access C413004q.Tq       := new C413004q.Tq;
      Q1_Cptr : access C413004q.Tq'Class := Q1_Ptr;
      Q2_Cptr : access C413004q.Tq'Class := Q2_Ptr;
   begin
      --  dispatching call to procedure (with no parameters)
      Q1_Cptr.Base_Proc;
      C413004q.Base_Proc (Q2_Cptr);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (dispatching call to procedure -1-)");
      end if;

      --  dispatching call to funcion (with no parameters)
      if Q1_Cptr.Base_Func /= C413004q.Base_Func (Q1_Cptr) then
         Report.Failed ("Wrong value (dispatching call to function -2-)");
      end if;

      -- call on inherited procedure (with parameters)
      Q1_Cptr.Base_Proc (Value => 123);
      if Q1_Cptr.Data /= 123 then
         Report.Failed ("Wrong value (inherited procedure -3-)");
      end if;

      --  call on inherited function (with parameters)
      if Q1_Cptr.Base_Func (Value => 1_234) /=
        C413004q.Base_Func (Q1_Ptr, 1_234) then
         Report.Failed ("Wrong value (inherited function -4-)");
      end if;

      --  dispatching call on new primitive function
      if Q1_Cptr.Prim_New_Func /= C413004q.Prim_New_Func (Q1_Cptr) then
         Report.Failed
           ("Wrong value (dispatching call on " & "primitive function)");
      end if;
   end;

   -- -------------------------------------------------------------------
   --  Test class-wide subprograms.

   declare
      Q_Ptr : access C413004q.Tq := new C413004q.Tq;
   begin
      --  call class-wide procedure (with no parameters)
      Q_Ptr.Class_Wide_Proc;                  --  Call P.Class_Wide_Proc
      if Q_Ptr.Data /= -1 then
         Report.Failed ("Wrong value (class-wide procedure -1-)");
      end if;

      --  call class-wide function (with no parameters)
      if Q_Ptr.Class_Wide_Func /= -2 then     --  Call P.Class_Wide_Func
         Report.Failed ("Wrong value (class-wide function -2-)");
      end if;

      --  call class-wide procedure (with parameters)
      Q_Ptr.Class_Wide_Proc (Value => 3);     --  Call P.Class_Wide_Proc
      if Q_Ptr.Data /= 9 then
         Report.Failed ("Wrong value (class-wide procedure -3-)");
      end if;

      --  call class-wide function (with parameters)
      if Q_Ptr.Class_Wide_Func (Value => 3) /= 9 then -- P.Call_Wide_Func
         Report.Failed ("Wrong value (class-wide function -4-)");
      end if;

      --  call class-wide procedure (with parameters)
      Q_Ptr.Class_Wide_Proc (Value => 3.0);
      --  C413004Q.Class_Wide_Proc
      if Q_Ptr.Value /= 9.0 then
         Report.Failed ("Wrong value (class-wide procedure -5-)");
      end if;

      --  call class-wide function (with parameters)
      if Q_Ptr.Class_Wide_Func (Value => 3.0) /= 9.0 then
         -- C413004Q.Call_Wide_Func
         Report.Failed ("Wrong value (class-wide function -6-)");
      end if;
   end;

   -- -------------------------------------------------------------------
   --  Test nested package.

   declare
      L1_Ptr : access C413004q.Local.Tpp := new C413004q.Local.Tpp;
      L2_Ptr : access C413004q.Local.Tpp := new C413004q.Local.Tpp;
   begin
      --  call on primitive procedure in nested package (with no parameters)
      L1_Ptr.Prim_Proc;
      C413004q.Local.Prim_Proc (L2_Ptr);
      if L1_Ptr.Data /= L2_Ptr.Data then
         Report.Failed ("Wrong value (primitive op in nested package -1-)");
      end if;

      --  call on primitive function in nested package (with no parameters)
      if L1_Ptr.Prim_Func /= C413004q.Local.Prim_Func (L1_Ptr) then
         Report.Failed ("Wrong value (primitive op in nested package -2-)");
      end if;

      --  call on primitive procedure in nested package (with parameters)
      L1_Ptr.Prim_Proc (Value => 123);
      C413004q.Local.Prim_Proc (L2_Ptr, Value => 123);
      if L1_Ptr.Data /= L2_Ptr.Data then
         Report.Failed ("Wrong value (primitive op in nested package -3-)");
      end if;

      --  call on primitive function in nested package (with parameters)
      if L1_Ptr.Prim_Func (Value => 123) /=
        C413004q.Local.Prim_Func (L1_Ptr, Value => 123) then
         Report.Failed ("Wrong value (primitive function -4-)");
      end if;
   end;

   Report.Result;
end C413004;

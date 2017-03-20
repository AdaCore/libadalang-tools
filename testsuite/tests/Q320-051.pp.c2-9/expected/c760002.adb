---------------------------------------------------------------- C760002

with Report;
with Tctouch;
with C760002_0;
with C760002_1;
with Ada.Finalization;
procedure C760002 is

   use type C760002_1.Proc_Id;

   -- in the first test, test the simple cases.
   -- Also check that assignment causes a call to Adjust for a controlled
   -- object.  Check that assignment of a non-controlled object does not call
   -- an Adjust procedure.

   procedure Check_Simple_Objects is

      A, B : C760002_0.Root;
      S, T : C760002_1.Test_Controlled;
      Q : C760002_1.Test_Limited_Controlled;  -- Adjust call shouldn't happen
   begin

      S := T;

      Tctouch.Assert
        ((S.Last_Proc_Called = C760002_1.Adj),
         "Adjust for simple object");
      Tctouch.Assert
        ((S.My_Id = T.My_Id),
         "Assignment failed for simple object");

      -- Check that adjust was called
      Tctouch.Assert ((S.Visit_Tag = 'A'), "Adjust timing incorrect");

      -- Check that Adjust has not been called
      Tctouch.Assert_Not ((T.Visit_Tag = 'A'), "Adjust incorrectly called");

      -- Check that Adjust does not get called
      A.My_Id := A.My_Id + 1;
      B       := A;  -- see: Adjust: Report.Failed

   end Check_Simple_Objects;

   -- in the second test, test a more complex case, check that a controlled
   -- component of a controlled object gets processed correctly

   procedure Check_Nested_Objects is
      No1 : C760002_1.Nested_Controlled;
      No2 : C760002_1.Nested_Controlled := No1;

   begin

      -- NO2 should be flagged with adjust markers
      Tctouch.Assert
        ((No2.Last_Proc_Called = C760002_1.Adj),
         "Adjust not called for NO2 enclosure declaration");
      Tctouch.Assert
        ((No2.Nested.Visit_Tag = 'A'),
         "Adjust not called for NO2 enclosed declaration");

      No2.Visit_Tag        := 'x';
      No2.Nested.Visit_Tag := 'y';

      No1 := No2;

      -- NO1 should be flagged with adjust markers
      Tctouch.Assert
        ((No1.Visit_Tag = 'A'),
         "Adjust not called for NO1 enclosure declaration");
      Tctouch.Assert
        ((No1.Nested.Visit_Tag = 'A'),
         "Adjust not called for NO1 enclosed declaration");

   end Check_Nested_Objects;

   procedure Check_Array_Case is
      type Array_Simple is array (1 .. 4) of C760002_1.Test_Controlled;
      type Array_Nested is array (1 .. 4) of C760002_1.Nested_Controlled;

      Left, Right : Array_Simple;
      Overlap     : Array_Simple := Left;

      Sinister, Dexter : Array_Nested;
      Underlap         : Array_Nested := Sinister;

      Now : Natural;

   begin

      -- get a current unique value since initializations
      Now := C760002_0.Unique_Value;

      -- check results of declarations
      for N in 1 .. 4 loop
         Tctouch.Assert
           (Left (N).My_Id < Now,
            "Initialize for array initial value");
         Tctouch.Assert
           (Overlap (N).My_Id < Now,
            "Adjust for nested array (outer) initial value");
         Tctouch.Assert
           (Sinister (N).Nested.My_Id < Now,
            "Initialize for nested array (inner) initial value");
         Tctouch.Assert
           (Sinister (N).My_Id < Sinister (N).Nested.My_Id,
            "Initialize for enclosure should be after enclosed");
         Tctouch.Assert (Overlap (N).Visit_Tag = 'A', "Adjust at declaration");
         Tctouch.Assert
           (Underlap (N).Nested.Visit_Tag = 'A',
            "Adjust at declaration, nested object");
      end loop;

      -- set visit tags
      for O in 1 .. 4 loop
         Overlap (O).Visit_Tag         := 'X';
         Underlap (O).Visit_Tag        := 'Y';
         Underlap (O).Nested.Visit_Tag := 'y';
      end loop;

      -- check that overlapping assignments don't cause odd grief
      Overlap (1 .. 3)  := Overlap (2 .. 4);
      Underlap (2 .. 4) := Underlap (1 .. 3);

      for M in 2 .. 3 loop
         Tctouch.Assert
           (Overlap (M).Last_Proc_Called = C760002_1.Adj,
            "Adjust for overlap");
         Tctouch.Assert (Overlap (M).Visit_Tag = 'A', "Adjust for overlap ID");
         Tctouch.Assert
           (Underlap (M).Last_Proc_Called = C760002_1.Adj,
            "Adjust for Underlap");
         Tctouch.Assert
           (Underlap (M).Nested.Visit_Tag = 'A',
            "Adjust for Underlaps nested ID");
      end loop;

   end Check_Array_Case;

   procedure Check_Access_Case is
      type Tc_Ref is access C760002_1.Test_Controlled;
      type Nc_Ref is access C760002_1.Nested_Controlled;
      type Tl_Ref is access C760002_1.Test_Limited_Controlled;
      type Nl_Ref is access C760002_1.Nested_Limited_Controlled;

      A, B : Tc_Ref;
      C, D : Nc_Ref;
      E    : Tl_Ref;
      F    : Nl_Ref;

   begin

      A := new C760002_1.Test_Controlled;
      B := new C760002_1.Test_Controlled'(A.all);

      C := new C760002_1.Nested_Controlled;
      D := new C760002_1.Nested_Controlled'(C.all);

      E := new C760002_1.Test_Limited_Controlled;
      F := new C760002_1.Nested_Limited_Controlled;

      Tctouch.Assert (A.Visit_Tag = 'I', "TC Allocation");
      Tctouch.Assert (B.Visit_Tag = 'A', "TC Allocation, with value");

      Tctouch.Assert (C.Visit_Tag = 'I', "NC Allocation");
      Tctouch.Assert (C.Nested.Visit_Tag = 'I', "NC Allocation, Nested");
      Tctouch.Assert (D.Visit_Tag = 'A', "NC Allocation, with value");
      Tctouch.Assert
        (D.Nested.Visit_Tag = 'A',
         "NC Allocation, Nested, with value");

      Tctouch.Assert (E.Visit_Tag = 'i', "TL Allocation");
      Tctouch.Assert (F.Visit_Tag = 'i', "NL Allocation");

      A.all := B.all;
      C.all := D.all;

      Tctouch.Assert (A.Visit_Tag = 'A', "TC Assignment");
      Tctouch.Assert (C.Visit_Tag = 'A', "NC Assignment");
      Tctouch.Assert (C.Nested.Visit_Tag = 'A', "NC Assignment, Nested");

   end Check_Access_Case;

   procedure Check_Access_Limited_Array_Case is
      type Array_Simple is array (1 .. 4) of C760002_1.Test_Limited_Controlled;
      type As_Ref is access Array_Simple;
      type Array_Nested is
        array (1 .. 4) of C760002_1.Nested_Limited_Controlled;
      type An_Ref is access Array_Nested;

      Simple_Array_Limited : As_Ref;

      Nested_Array_Limited : An_Ref;

   begin

      Simple_Array_Limited := new Array_Simple;

      Nested_Array_Limited := new Array_Nested;

      for N in 1 .. 4 loop
         Tctouch.Assert
           (Simple_Array_Limited (N).Last_Proc_Called = C760002_1.Init,
            "Initialize for array initial value");
         Tctouch.Assert
           (Nested_Array_Limited (N).Last_Proc_Called = C760002_1.Init,
            "Initialize for nested array (outer) initial value");
         Tctouch.Assert
           (Nested_Array_Limited (N).Nested.Visit_Tag = 'i',
            "Initialize for nested array (inner) initial value");
      end loop;
   end Check_Access_Limited_Array_Case;

begin  -- Main test procedure.

   Report.Test
     ("C760002",
      "Check that assignment causes the Adjust " &
      "operation of the type to be called.  Check " &
      "that Adjust is called after copying the " &
      "value of the source expression to the target " &
      "object.  Check that Adjust is called for all " &
      "controlled components when the containing " &
      "object is assigned.  Check that Adjust is " &
      "called for components before the containing " &
      "object is adjusted.  Check that Adjust is not " &
      "called for a Limited_Controlled type by the " &
      "implementation");

   Check_Simple_Objects;

   Check_Nested_Objects;

   Check_Array_Case;

   Check_Access_Case;

   Check_Access_Limited_Array_Case;

   Report.Result;

end C760002;

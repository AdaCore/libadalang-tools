-----------------------------------------------------------------------------

with Cxe4004_Common; use Cxe4004_Common;
with Cxe4004_Shared; use Cxe4004_Shared;
with Cxe4004_Part_A1;
with Cxe4004_Part_A2;
with Cxe4004_Part_B;
with Report;
procedure Cxe4004_B is
   function "+"
     (L, R : Cxe4004_Common.Integer_Vector) return Cxe4004_Common
     .Integer_Vector renames
     Cxe4004_Part_A1."+";

   procedure Chk is new Cxe4004_Shared.Gen_Chk (Integer);
   procedure Chk is new Cxe4004_Shared.Gen_Chk (Boolean);
   procedure Chk is new Cxe4004_Shared.Gen_Chk (Character);

--------------------------------------------

   procedure In_Tests is
      V               : Integer_Vector (7 .. 9) := (77, 88, 99);
      Not_Constrained : Discriminated_Record := (False, 7, (5, 4, 3), 2);
      Simple_Dr2      : Another_Discriminated_Record (True, 0, 0) :=
        (True, 0, 0, 98, A_Char => 'p');
      Tagged_Rec : Extended_2;
   begin
      Tagged_Rec.In_Root         := 56;
      Tagged_Rec.Ext_2_Component := 57;

      -- Test #1
      --    check that the bounds for V are 7 and 9
      --    check the discriminant and component values for everything else
      Cxe4004_Part_A1.Check_In (1, V, Not_Constrained, Simple_Dr2, Tagged_Rec);

      -- Test #2
      --    check that the bounds of the array components of
      --    Another_Discriminated_Record are correct and the values in the
      --    arrays are correct
      Cxe4004_Part_A1.Check_In
        (2, V, Not_Constrained,
         Another_Discriminated_Record'
           (False, Disc_Low => 3, Disc_High => 5, Common_Component => 18,
            A_Bool          => False, Some_Ints => (3 => 30, 4 => 40, 5 => 50),
            Some_More_Ints  => (1, 2, 3, 4, 5),
            Even_More_Ints  => (3 .. 19 => 319, 20 => 200)),
         Tagged_Rec);
   end In_Tests;

--------------------------------------------

   procedure Out_Tests is

   begin
      -- Test #1
      --    check the Constrained attribute on the other side.
      --       Both records are constrained.
      --    check a few of the values returned
      declare
         Vec : Integer_Vector (18 .. 19);
         Dr1 : Discriminated_Record (True);
         Dr2 : Another_Discriminated_Record (False, 2, 3);
         Tr  : Extended_2;
      begin
         Cxe4004_Part_A1.Set_Out (1, Vec, Dr1, Dr2, Tr);

         Chk ("Out_Tests.Vec(18)", Vec (18), 1_800);
         Chk ("Out_Tests.Dr1.A_Char", Dr1.A_Char, 'Z');
         Chk
           ("Out_Tests.Dr2.Some_More_Ints(2)", Dr2.Some_More_Ints (2), 2_222);
         Chk ("Out_Tests.Tr.In_Root", Tr.In_Root, 3_333);
      end;

      -- Test #2
      --    check the Constrained attribute on the other side.
      --       Dr1 is not constrained.  Dr2 is constrained.
      --    check a few of the values returned
      declare
         Vec : Integer_Vector (-5 .. -3);
         Dr1 : Discriminated_Record;  -- use default disc.
         Dr2 : Another_Discriminated_Record (False, 2, 5);
         Tr  : Extended_2;
      begin
         Cxe4004_Part_A1.Set_Out (2, Vec, Dr1, Dr2, Tr);

         Chk ("Out_Tests.Vec(-5)", Vec (-5), -5);
         Chk ("Out_Tests.Vec(-3)", Vec (-3), -3);
         Chk ("Out_Tests.Dr2.Some_Ints(5)", Dr2.Some_Ints (5), 255);
      end;

   end Out_Tests;

--------------------------------------------

   procedure Inout_Tests is
      V               : Integer_Vector (12 .. 15)    := (177, 188, 199, 166);
      Big_V           : Integer_Vector (1_000 .. 2_500);
      Not_Constrained : Discriminated_Record := (False, 17, (15, 24, 33), 42);
      Is_Constrained  : Discriminated_Record (False) :=
        (False, 18, (115, 124, 133), 142);
      Another_Dr : Another_Discriminated_Record (True, 9, 10) :=
        (True, 9, 10, 998, A_Char => 'q');
      A_Big_Dr : Another_Discriminated_Record (False, 19, 419) :=
        (False, 19, 419, Common_Component => 400,
         Some_Ints => (19 .. 419 => 10_101), A_Bool => True,
         Some_More_Ints                   => (1 .. 419 => 10_202),
         Even_More_Ints                   => (19 .. 20 => 10_303));
      Tagged_Rec : Extended_2;
   begin
      Tagged_Rec.In_Root         := 678;
      Tagged_Rec.Ext_2_Component := 901;

      -- Test #1
      --    check that the Constrained attribute is passed correctly
      --    check some of the modified values that are returned
      --    check some of the unmodified values too.
      Cxe4004_Part_A1.Modify (1, V, Not_Constrained, Another_Dr, Tagged_Rec);

      -- not modified by call
      Chk ("InOut_Tests.V(14)", V (14), 199);
      Chk ("InOut_Tests.Tagged_Rec.In_Root", Tagged_Rec.In_Root, 678);
      Chk
        ("InOut_Tests.Not_Constrained.Common_Component",
         Not_Constrained.Common_Component, 17);
      Chk
        ("InOut_Tests.Another_DR.Common_Component",
         Another_Dr.Common_Component, 998);

      -- modified by call
      Chk ("InOut_Tests.V(13)", V (13), 881);
      Chk ("InOut_Tests.Not_Constrained.Disc", Not_Constrained.Disc, True);
      Chk ("InOut_Tests.Another_DR.A_Char", Another_Dr.A_Char, 's');
      Chk
        ("InOut_Tests.Tagged_Rec.Ext_2_Component", Tagged_Rec.Ext_2_Component,
         109);

      -- Test #2
      --    make the parameters bigger
      Tagged_Rec.In_Root         := 1_022;
      Tagged_Rec.Ext_2_Component := 1_124;
      for I in Big_V'Range loop
         Big_V (I) := I;
      end loop;
      for I in A_Big_Dr.Even_More_Ints'Range loop
         A_Big_Dr.Even_More_Ints (I) := -I;
      end loop;

      Cxe4004_Part_A1.Modify (2, Big_V, Is_Constrained, A_Big_Dr, Tagged_Rec);

      -- not modified by call
      Chk ("InOut_Tests.Big_V(2000)", Big_V (2_000), 2_000);
      Chk
        ("InOut_Tests.Is_Constrained.Another_Int", Is_Constrained.Another_Int,
         142);
      Chk
        ("InOut_Tests.A_Big_DR.Even_More_Ints(20)",
         A_Big_Dr.Even_More_Ints (20), -20);
      Chk
        ("InOut_Tests.Tagged_Rec.Ext_2_Component", Tagged_Rec.Ext_2_Component,
         1_124);

      -- modified by call
      Chk ("InOut_Tests.Big_V(1900)", Big_V (1_900), 135);
      Chk
        ("InOut_Tests.Is_Constrained.Ints_3(5)", Is_Constrained.Ints_3 (5),
         35);
      Chk
        ("InOut_Tests.A_Big_DR.Even_More_Ints(19)",
         A_Big_Dr.Even_More_Ints (19), 190);
      Chk ("InOut_Tests.Tagged_Rec.In_Root", Tagged_Rec.In_Root, 2_201);

   end Inout_Tests;

--------------------------------------------

   procedure Large_Array_Tests is
      Ones : constant Integer_Vector (1_000 .. 3_000) := (1_000 .. 3_000 => 1);
      Twos : constant Integer_Vector (1_000 .. 3_000) := (1_000 .. 3_000 => 2);
      Fives : constant Integer_Vector (1_000 .. 3_000) :=
        (1_000 .. 3_000 => 5);
      Result : Integer_Vector (1_000 .. 3_000) := (1_000 .. 3_000 => 0);
   begin
      Start_Test_Section ("large array parameters and function results");
      Result := (Twos + Ones) + Twos;
      if Result = Fives then
         null;
      else
         Report.Failed
           ("large array parameters and/or array function results");
      end if;
   end Large_Array_Tests;

--------------------------------------------

   procedure Remote_Access_Tests is
      -- access to remote subprogram tests here we make sure the correct
      -- procedure is called by having several procedures with the same
      -- parameter profile but each procedure expects a different value to
      -- be passed to it as is indicated by the procedure name.

      P2, P3, P4 : Cxe4004_Part_A2.Remote_Proc;
   begin
      Start_Test_Section ("remote call via an access to subprogram");
      P2 := Cxe4004_Part_A2.Call_With_2'Access;
      P3 := Cxe4004_Part_A2.Call_With_3'Access;
      P4 := Cxe4004_Part_B.Call_With_4'Access;
      -- try two different RCI packages that are in the same partition
      P2 ((1 .. 2 => 2));
      P3 ((101 .. 103 => 3));   -- do some sliding too
      -- try a procedure that is in this partition
      P4 ((1 .. 4 => 4));
   end Remote_Access_Tests;

--------------------------------------------

begin  -- CXE4004_B
   Report.Test ("CXE4004_B", "Parameter passing across partitions");

   -- make sure partitioning was performed correctly
   if Cxe4004_Part_A1'Partition_Id = Cxe4004_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE4004_Part_A1 and CXE4004_B" &
         " are in the same partition.");
   end if;
   if Cxe4004_Part_A2'Partition_Id = Cxe4004_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE4004_Part_A2 and CXE4004_B" &
         " are in the same partition.");
   end if;
   if Cxe4004_Part_B'Partition_Id /= Cxe4004_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE4004_Part_B and CXE4004_B" &
         " are not in the same partition.");
   end if;

   -- do the tests
   In_Tests;
   Out_Tests;
   Inout_Tests;
   Large_Array_Tests;
   Remote_Access_Tests;

   -- finish up
   Cxe4004_Part_A1.Quit;
   Report.Result;
end Cxe4004_B;

-----------------------------------------------------------------------------

with Cxe4002_Common;
with Cxe4002_Part_A1;
with Cxe4002_Part_A2;
with Report;
procedure Cxe4002_B is
   function "=" (L, R : Cxe4002_Common.Integer_Vector) return Boolean renames
     Cxe4002_Common."=";
   function "+"
     (L,
      R : Cxe4002_Common.Integer_Vector)
      return Cxe4002_Common
     .Integer_Vector renames
     Cxe4002_Part_A1."+";
   function "=" (L, R : Cxe4002_Common.Little_Number) return Boolean renames
     Cxe4002_Common."=";

   use type Cxe4002_Common.Record_Data;
begin
   Report.Test ("CXE4002_B", "Parameter passing across partitions");

   -- make sure partitioning is performed
   if Cxe4002_Part_A1'Partition_Id = Cxe4002_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE4002_Part_A1 and CXE4002_B" &
         " are in the same partition.");
   end if;

   -- do the tests

   -- simple IN parameter test
   Cxe4002_Part_A1.Check_In (1, 2.0, 3);

   -- simple OUT and IN OUT parameter test
   declare
      A : Cxe4002_Common.Little_Number;
      B : Float;
      C : Integer;
   begin
      Cxe4002_Part_A1.Set_Out (A, B, C);
      if A /= 4 or B /= -123.0 or C /= -789 then
         Report.Failed ("OUT parameters not set properly");
      end if;

      A := 6;
      B := 2.0;
      C := -1;
      Cxe4002_Part_A1.Decr (A, B, C);
      if A = 5 and B = 1.0 and C = -2 then
         null;
         -- Report.Comment ("finished simple parameter passing");
      else
         Report.Failed ("IN OUT parameters not returned properly");
      end if;
   end;

   -- do the record type tests now
   declare
      I_Data : Cxe4002_Common.Record_Data;
   begin
      I_Data := Cxe4002_Part_A1.Current_Record ("1234567890");
      if I_Data /= (1, 198.0, "1234567890") then
         Report.Failed ("composite function result not the expected value");
      end if;

      Cxe4002_Part_A1.Update_Record ((22, 33.0, "abcdefghij"), I_Data);
      if I_Data.Part_No /= 24 then
         Report.Failed ("OUT parameter Part_No component has wrong value");
      end if;
      if I_Data.Cost /= 66.0 then
         Report.Failed ("OUT parameter Cost component has wrong value");
      end if;
      if I_Data.Name /= "ABCDEFGHIJ" then
         Report.Failed ("OUT parameter Name component has wrong value");
      else
         null;
         -- Report.Comment ("OUT parameter tests");
      end if;
   end;

   -- do the array type tests now
   declare
      Ones   : constant Cxe4002_Common.Integer_Vector := (others => 1);
      Twos   : constant Cxe4002_Common.Integer_Vector := (others => 2);
      Fives  : constant Cxe4002_Common.Integer_Vector := (others => 5);
      Result : Cxe4002_Common.Integer_Vector          := (others => 0);
   begin
      Result := (Twos + Ones) + Twos;
      if Result = Fives then
         null;
         -- Report.Comment ("array parameter and function result");
      else
         Report.Failed
           ("incorrect array parameters and/or" & " array function results");
      end if;

      Result := Ones;
      Cxe4002_Part_A1.Incr_Vector (Result);
      if Result /= Twos then
         Report.Failed ("incorrect array IN OUT parameter");
      end if;
   end;

   -- access to remote subprogram tests here we make sure the correct procedure
   -- is called by having several procedures with the same parameter profile
   -- but each procedure expects a different value to be passed to it as is
   -- indicated by the procedure name.
   declare
      P2, P3, P4 : Cxe4002_Part_A2.Remote_Proc;
   begin
      P2 := Cxe4002_Part_A2.Call_With_2'Access;
      P3 := Cxe4002_Part_A2.Call_With_3'Access;
      P4 := Cxe4002_Part_A1.Call_With_4'Access;
      -- try two different procedures from the same RCI package
      P2 (2);
      P3 (3);
      -- try a procedure that is in a different RCI package
      P4 (4);
   end;

   -- access to remote subprogram tests with mixed parameters. make sure the
   -- pointer is used.
   declare
      M1 : Cxe4002_Part_A2.Remote_Proc_Mixed;
      M2 : Cxe4002_Part_A2.Remote_Proc_Mixed;
      T  : Cxe4002_Part_A2.Remote_Proc_Mixed;

      D, E : Integer := 33;

   begin
      T := Cxe4002_Part_A2.Mixed_1'Access;
      if Report.Ident_Int (1) = 1 then
         M1 := T;
         M2 := Cxe4002_Part_A2.Mixed_2'Access;
      else -- not executed
         M2 := T;
         M1 := Cxe4002_Part_A2.Mixed_2'Access;
      end if;
      E := 30;
      M1 (20, D, E);
      if D /= 25 or E /= 35 then
         Report.Failed
           ("OUT parameters from Mixed 1 are not the" & " expected values");
      end if;

      E := 300;
      D := 100;
      M2 (200, D, E);
      if D /= 250 or E /= 350 then
         Report.Failed
           ("OUT parameters from Mixed 2 are not the" & " expected values");
      end if;
   end;

   -- finish up
   Cxe4002_Part_A1.Quit;
   Report.Result;
end Cxe4002_B;

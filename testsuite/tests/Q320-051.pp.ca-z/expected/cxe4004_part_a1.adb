-----------------------------------------------------------------------------

with Report;
with Cxe4004_Shared; use Cxe4004_Shared;
package body Cxe4004_Part_A1 is

   procedure Chk is new Cxe4004_Shared.Gen_Chk (Integer);
   procedure Chk is new Cxe4004_Shared.Gen_Chk (Boolean);
   procedure Chk is new Cxe4004_Shared.Gen_Chk (Character);

   procedure Check_In
     (Test_No : in Integer; Vec : in Integer_Vector;
      Dr1     : in Discriminated_Record; Dr2 : in Another_Discriminated_Record;
      Tr      : in Extended_2)
   is
   begin
      Start_Test_Section ("IN parameter test #" & Integer'Image (Test_No));
      -- check a sample of the data that came in
      case Test_No is
         when 1 =>
            Chk ("Vec'First", Vec'First, 7);
            Chk ("Vec'Last", Vec'Last, 9);
            Chk ("Vec(7)", Vec (7), 77);
            Chk ("Vec(8)", Vec (8), 88);
            Chk ("Vec(9)", Vec (9), 99);
            Chk ("Dr1.Disc", Dr1.Disc, False);
            Chk ("Dr1.Common_Component", Dr1.Common_Component, 7);
            Chk ("Dr1.Another_Int", Dr1.Another_Int, 2);
            Chk ("Dr2.Disc", Dr2.Disc, True);
            Chk ("Dr2.Disc_Low", Dr2.Disc_Low, 0);
            Chk ("Dr2.Disc_High", Dr2.Disc_High, 0);
            Chk ("Dr2.A_Char", Dr2.A_Char, 'p');
            Chk ("Tr.In_Root", Tr.In_Root, 56);
            Chk ("Tr.Ext_2_Component", Tr.Ext_2_Component, 57);

         when 2 =>
            Chk ("Dr2.Disc", Dr2.Disc, False);
            Chk ("Dr2.Disc_Low", Dr2.Disc_Low, 3);
            Chk ("Dr2.Disc_High", Dr2.Disc_High, 5);
            Chk ("Dr2.Some_Ints(3)", Dr2.Some_Ints (3), 30);
            Chk ("Dr2.Some_Ints(5)", Dr2.Some_Ints (5), 50);
            Chk ("Dr2.Some_More_Ints(3)", Dr2.Some_More_Ints (3), 3);
            Chk ("Dr2.Even_More_Ints(19)", Dr2.Even_More_Ints (19), 319);
            Chk ("Dr2.Even_More_Ints(20)", Dr2.Even_More_Ints (20), 200);

         when others =>
            Report.Failed
              ("Bad test number received for Check_In" &
               Integer'Image (Test_No));
      end case;
   end Check_In;

   procedure Set_Out
     (Test_No : in     Integer; Vec : out Integer_Vector;
      Dr1 : out Discriminated_Record; Dr2 : out Another_Discriminated_Record;
      Tr      :    out Extended_2)
   is
   begin
      Start_Test_Section ("OUT parameter test #" & Integer'Image (Test_No));

      -- check attributes and set the values expected by the caller
      case Test_No is
         when 1 =>
            Chk ("Vec'First", Vec'First, 18);
            Chk ("Dr1'Constrained", Dr1'Constrained, True);
            Chk ("Dr2'Constrained", Dr2'Constrained, True);
            Chk ("Dr2.Disc_Low", Dr2.Disc_Low, 2);

            Vec (18 .. 19)              := (1_800, 1_801);
            Dr1.A_Char                  := 'Z';
            Dr2.Some_More_Ints (1 .. 3) := (2_221, 2_222, 2_223);
            Tr.In_Root                  := 3_333;
            Tr.Ext_2_Component          := 3_334;

         when 2 =>
            Chk ("Vec'First", Vec'First, -5);
            Chk ("Dr1'Constrained", Dr1'Constrained, False);
            Chk ("Dr2'Constrained", Dr2'Constrained, True);
            Chk ("Dr2.Disc_Low", Dr2.Disc_Low, 2);

            Vec (-5 .. -3)         := (-5, -4, -3);
            Dr2.Some_Ints (2 .. 5) := (252, 253, 254, 255);

         when others =>
            Report.Failed
              ("Bad test number received for Set_Out" &
               Integer'Image (Test_No));
      end case;
   end Set_Out;

   procedure Modify
     (Test_No : in     Integer; Vec : in out Integer_Vector;
      Dr1     : in out Discriminated_Record;
      Dr2     : in out Another_Discriminated_Record; Tr : in out Extended_2)
   is
   begin
      Start_Test_Section ("IN OUT Parameter test #" & Integer'Image (Test_No));

      -- check attributes and set the values expected by the caller
      case Test_No is
         when 1 =>
            -- check the attributes
            Chk ("Vec'First", Vec'First, 12);
            Chk ("Dr1'Constrained", Dr1'Constrained, False);
            Chk ("Dr2.Disc_Low", Dr2.Disc_Low, 9);
            -- check the values we are going to modify
            Chk ("Vec(13)", Vec (13), 188);
            Chk ("Dr1.Disc", Dr1.Disc, False);
            Chk ("Dr2.A_Char", Dr2.A_Char, 'q');
            Chk ("Tr.Ext_2_Component", Tr.Ext_2_Component, 901);
            -- modify the values we want to send back
            Vec (13)           := 881;
            Dr1                := (True, 17, 't');
            Dr2.A_Char         := 's';
            Tr.Ext_2_Component := 109;

         when 2 =>
            -- check the attributes
            Chk ("Vec'First", Vec'First, 1_000);
            Chk ("Dr1'Constrained", Dr1'Constrained, True);
            Chk ("Dr2.Disc_Low", Dr2.Disc_Low, 19);
            -- check the values we are going to modify
            Chk ("Vec(1900)", Vec (1_900), 1_900);
            Chk ("Dr1.Ints_3(5)", Dr1.Ints_3 (5), 124);
            Chk ("Dr2.Even_More_Ints(19)", Dr2.Even_More_Ints (19), -19);
            Chk ("Tr.In_Root", Tr.In_Root, 1_022);
            -- modify the values we want to send back
            Vec (1_900)             := 135;
            Dr1.Ints_3 (5)          := 35;
            Dr2.Even_More_Ints (19) := 190;
            Tr.In_Root              := 2_201;

         when others =>
            Report.Failed
              ("Bad test number received for Modify" &
               Integer'Image (Test_No));
      end case;
   end Modify;

   -- vector operation tests
   function "+" (A, B : in Integer_Vector) return Integer_Vector is
      Result : Integer_Vector (A'Range);
   begin
      for I in Result'Range loop
         Result (I) := A (I) + B (I);
      end loop;
      return Result;
   end "+";

   ---------  partition termination coordination ----------
   -- use a task to prevent the partition from completing its execution until
   -- the main procedure in partition B tells it to quit.

   task Wait_For_Quit is
      entry Can_Quit;
      entry Quit;
   end Wait_For_Quit;

   task body Wait_For_Quit is
   begin
      accept Can_Quit; -- Called once we've called Report.Test. (Else we might
      -- call Report.Result before Report.Test.)
      accept Quit;
      Report.Result;
   end Wait_For_Quit;

   procedure Can_Quit is
   begin
      Wait_For_Quit.Can_Quit;
   end Can_Quit;

   procedure Quit is
   begin
      Wait_For_Quit.Quit;
   end Quit;

end Cxe4004_Part_A1;

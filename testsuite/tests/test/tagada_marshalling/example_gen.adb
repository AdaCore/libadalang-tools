with Ada.Directories;             use Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;   use Ada.Numerics.Float_Random;
with Ada.Streams;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces;                  use Interfaces;
with My_File;                     use My_File;
with My_File.TGen_Support;        use My_File.TGen_Support;
with TGen.TGen_Support;           use TGen.TGen_Support;
with Show_Date;                   use Show_Date;
with Show_Date.TGen_Support;      use Show_Date.TGen_Support;

procedure Example_Gen is

   package Rand1 is new Ada.Numerics.Discrete_Random (T1);
   Gen1 : Rand1.Generator;
   package Rand2 is new Ada.Numerics.Discrete_Random (T2);
   Gen2 : Rand2.Generator;
   package Rand3 is new Ada.Numerics.Discrete_Random (T3);
   Gen3 : Rand3.Generator;
   package Rand4 is new Ada.Numerics.Discrete_Random (Boolean);
   Gen4 : Rand4.Generator;
   Gen_Float : Ada.Numerics.Float_Random.Generator;
   package Rand5 is new Ada.Numerics.Discrete_Random (Positive);
   Gen5 : Rand5.Generator;
   subtype OK_Char is Character range ' ' .. '~';
   package Rand6 is new Ada.Numerics.Discrete_Random (OK_Char);
   Gen6 : Rand6.Generator;

   package Rand7 is new Ada.Numerics.Discrete_Random (Name_Size_Ty);
   Gen7 : Rand7.Generator;
   package Rand8 is new Ada.Numerics.Discrete_Random (Shape_Kind);
   Gen8 : Rand8.Generator;

   subtype Small_Int is Integer range -100 .. 100;
   package Rand9 is new Ada.Numerics.Discrete_Random (Small_Int);
   Gen9 : Rand9.Generator;

   procedure Put_Shape (V : Shape) is
   begin
      Ada.Text_IO.Put
        (V.Name & " (" &V.K'Image & "): X => "
         & V.X'Image & ", Y => " & V.Y'Image);
      case V.K is
         when Point =>
            null;
         when Line  =>
            Ada.Text_IO.Put
              (", X2 => " & V.X_2'Image & ", Y2 => " & V.Y_2'Image);
         when Circle =>
            Ada.Text_IO.Put (", Radius => " & V.Radius'Image);
         when Ellipse =>
            declare
               Small : constant Integer := Integer'Min (V.Radius, V.Radius_2);
               Big   : constant Integer := Integer'Max (V.Radius, V.Radius_2);
            begin
               Ada.Text_IO.Put
                 (", Minor Axis => " & Small'Image
                  & ", Major Axis => " & Big'Image);
            end;
         when Square =>
            Ada.Text_IO.Put (", Side => " & V.Side'Image);
         when Rectangle =>
            Ada.Text_IO.Put
              (", Width => " & V.Side'Image
               & ", Length => " & V.Side_2'Image);
      end case;
      Ada.Text_IO.New_Line;
   end Put_Shape;

   procedure Put_R (V : R) is
   begin
      Ada.Text_IO.Put_Line ("F1 => " & V.F1'Image);
      Ada.Text_IO.Put_Line ("F2 => " & V.F2'Image);
      Ada.Text_IO.Put_Line ("F3 => " & V.F3'Image);
      Ada.Text_IO.Put_Line ("F4 => " & V.F4'Image);
      Ada.Text_IO.Put_Line ("F5 => " & V.F5'Image);
      Ada.Text_IO.Put_Line ("F6 => " & V.F6'Image);
      Ada.Text_IO.Put_Line ("F7 => " & V.F7'Image);
      Ada.Text_IO.Put ("F8 => (");
      for I in 1 .. 10 loop
         Ada.Text_IO.Put (V.F8 (I)'Image);
         if I /= 10 then
            Ada.Text_IO.Put (", ");
         end if;
      end loop;
      Ada.Text_IO.Put_Line (")");
      Ada.Text_IO.Put_Line ("F9 => " & V.F9'Image);
      Ada.Text_IO.Put_Line ("G1 => " & V.G1'Image);
      Ada.Text_IO.Put_Line ("G2 => " & V.G2'Image);
      Ada.Text_IO.Put ("G3 => ");
      for I in V.G3'Range (1) loop
         for J in V.G3'Range (2) loop
            Ada.Text_IO.Put (if V.G3 (I, J) then "x " else "  ");
         end loop;
         Ada.Text_IO.New_Line;
         if I /= V.G3'Last (1) then
            Ada.Text_IO.Put ("      ");
         end if;
      end loop;
      Ada.Text_IO.Put ("G4 => ");
      Put_Shape (V.G4);
      Ada.Text_IO.Put ("G5 => ");
      Put_Shape (V.G5);
      Ada.Text_IO.Put_Line ("G6 => ");
      for I in 1 .. V.G6.L loop
         Ada.Text_IO.Put ("   ");
         Put_Shape (V.G6.Content (I));
      end loop;
   end Put_R;

   function Random_Shape return Shape is
      K : constant Shape_Kind := Rand8.Random (Gen8);
   begin
      case K is
         when Point     =>
            return (Point, 8, "my_point", others => Rand9.Random (Gen9));
         when Line      =>
            return (Line, 7, "my_line", others => Rand9.Random (Gen9));
         when Circle    =>
            return (Circle, 9, "my_circle", Rand9.Random (Gen9), Rand9.Random (Gen9), others => Rand5.Random (Gen5));
         when Ellipse   =>
            return (Ellipse, 10, "my_ellipse", Rand9.Random (Gen9), Rand9.Random (Gen9), others => Rand5.Random (Gen5));
         when Square    =>
            return (Square, 9, "my_square", Rand9.Random (Gen9), Rand9.Random (Gen9), others => Rand5.Random (Gen5));
         when Rectangle =>
            return (Rectangle, 12, "my_rectangle", Rand9.Random (Gen9), Rand9.Random (Gen9), others => Rand5.Random (Gen5));
      end case;
   end Random_Shape;

   procedure Test (Debug : Boolean := False) is
      F         : Ada.Streams.Stream_IO.File_Type;
      S         : Stream_Access;
      File_Name : constant String := "scratch_pad.bin";
      V1        : constant R :=
        (F1 => Rand1.Random (Gen1),
         F2 => Rand2.Random (Gen2),
         F3 => Rand3.Random (Gen3),
         F4 => Rand4.Random (Gen4),
         F5 => (if Rand4.Random (Gen4) then Random (Gen_Float) * Float'Last
                else Random (Gen_Float) * Float'First),
         F6 => (if Rand4.Random (Gen4) then Long_Float (Random (Gen_Float)) * Long_Float'Last
                else Long_Float (Random (Gen_Float)) * Long_Float'First),
         F7 => (if Rand4.Random (Gen4) then Long_Float (Random (Gen_Float)) * Long_Float'Last
                else Long_Float (Random (Gen_Float)) * Long_Float'First),
         F8 => (1 .. 10 => Rand5.Random (Gen5)),
         F9 => Rand6.Random (Gen6),
         G1 => (if Rand4.Random (Gen4) then Fixed_1 (Random (Gen_Float) * Float (Fixed_1'Last))
                else Fixed_1 (Random (Gen_Float) * Float (Fixed_1'First))),
         G2 => (if Rand4.Random (Gen4) then Fixed_2 (Random (Gen_Float) * Float (Fixed_2'Last))
                else Fixed_2 (Random (Gen_Float) * Float (Fixed_2'First))),
         G3 => (others => (others => Rand4.Random (Gen4))),
         G4 => (Ellipse, 10, "my_ellipse", Rand9.Random (Gen9), Rand9.Random (Gen9), others => Rand5.Random (Gen5)),
         G5 => Random_Shape,
         G6 => (declare
                L : constant T2 := Rand2.Random (Gen2);
                begin
                Small_Shape_Array'(L => L, Content => (1 .. L => Random_Shape))));
      V2        : R;

      D1 : constant Date := (Year => 2033, Month => January, Day => 23);
      D2 : Date;
   begin
      if Debug then
         Put_R (V1);
      end if;

      Create (F, Out_File, File_Name);
      S := Stream (F);
      TGen_Marshalling_My_File_R_Output (S, V1);
      TGen_Marshalling_Show_Date_Date_Output (S, D1);
      Close (F);

      Open (F, In_File, File_Name);
      S := Stream (F);
      V2 := TGen_Marshalling_My_File_R_Input (S);
      D2 := TGen_Marshalling_Show_Date_Date_Input (S);
      Close (F);

      if Debug then
         Put_R (V2);
      end if;

      if V1 /= V2 then
         Ada.Text_IO.Put_Line ("V1 FAIL");
      end if;
      if D1 /= D2 then
         Ada.Text_IO.Put_Line ("D1 FAIL");
      end if;
   end Test;

   procedure Test_Discr (Debug : Boolean := False) is
      F          : Ada.Streams.Stream_IO.File_Type;
      S          : Stream_Access;
      File_Name  : constant String := "scratch_pad.bin";
      V1         : constant Shape := Random_Shape;
   begin
      if Debug then
         Put_Shape (V1);
      end if;

      Create (F, Out_File, File_Name);
      S := Stream (F);
      TGen_Marshalling_My_File_Shape_Output (S, V1);
      Close (F);

      Open (F, In_File, File_Name);
      S := Stream (F);
      declare
         V2 : Shape := TGen_Marshalling_My_File_Shape_Input (S);
      begin
         Close (F);

         if Debug then
            Put_Shape (V2);
         end if;

         if V1 /= V2 then
            Ada.Text_IO.Put_Line ("Discr FAIL");
         end if;
      end;
   end Test_Discr;

   procedure Test_String (Debug : Boolean := False) is
      F          : Ada.Streams.Stream_IO.File_Type;
      S          : Stream_Access;
      File_Name  : constant String := "scratch_pad.bin";
      Fst        : constant Positive := 1 + Rand7.Random (Gen7);
      Lgth       : constant Natural := Rand7.Random (Gen7);
      V1         : constant String :=
        (Fst .. Fst + Lgth => Rand6.Random (Gen6));
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("First => " & V1'First'Image);
         Ada.Text_IO.Put_Line ("Last  => " & V1'Last'Image);
         Ada.Text_IO.Put_Line ("Value => " & V1);
      end if;

      Create (F, Out_File, File_Name);
      S := Stream (F);
      TGen_Marshalling_Standard_String_Output (S, V1);
      Close (F);

      Open (F, In_File, File_Name);
      S := Stream (F);
      declare
         V2 : String := TGen_Marshalling_Standard_String_Input (S);
      begin
         Close (F);

         if Debug then
            Ada.Text_IO.Put_Line ("First => " & V2'First'Image);
            Ada.Text_IO.Put_Line ("Last  => " & V2'Last'Image);
            Ada.Text_IO.Put_Line ("Value => " & V2);
         end if;

         if V1 /= V2 then
            Ada.Text_IO.Put_Line ("String FAIL");
         end if;
      end;
   end Test_String;

   procedure Test_Pred is
      F          : Ada.Streams.Stream_IO.File_Type;
      S          : Stream_Access;
      File_Name  : constant String := "scratch_pad.bin";
      V          : R2 := (others => True);
      W          : R2 := V;
   begin
      Create (F, Out_File, File_Name);
      S := Stream (F);
      TGen_Marshalling_My_File_R2_Output (S, V);
      Close (F);

      Open (F, In_File, File_Name);
      S := Stream (F);
      W := TGen_Marshalling_My_File_R2_Input (S);

      if V /= W then
         Ada.Text_IO.Put_Line ("Preds FAIL");
      end if;
   end Test_Pred;

begin
   Rand1.Reset (Gen1);
   Rand2.Reset (Gen2);
   Rand3.Reset (Gen3);
   Rand4.Reset (Gen4);
   Reset (Gen_Float);
   Rand5.Reset (Gen5);
   Rand6.Reset (Gen6);
   Rand7.Reset (Gen7);
   Rand8.Reset (Gen8);

   --  Generate records of type R randomly, marshall them and unmarshall them

   for I in 1 .. 5000 loop
      Test;
   end loop;

   --  Generate strings of various sizes randomly, marshall them and unmarshall them

   for I in 1 .. 5000 loop
      Test_String;
   end loop;

   --  Generate shapes of various kinds randomly, marshall them and unmarshall them

   for I in 1 .. 5000 loop
      Test_Discr;
   end Loop;

   --  The default value of R2 breaks its predicate. Test that we can still
   --  Marshall Valid Values of this type.

   Test_Pred;

   Ada.Text_IO.Put_Line
     ("Size of String headers in bytes:"
      & TGen_Marshalling_standard_String_Byte_Size_Header'Image);
   Ada.Text_IO.Put_Line
     ("Size of Matrix headers in bytes:"
      & TGen_Marshalling_my_file_Matrix_Byte_Size_Header'Image);
   Ada.Text_IO.Put_Line
     ("Size of Shape headers in bytes:"
      & TGen_Marshalling_my_file_Shape_Byte_Size_Header'Image);
   Ada.Text_IO.Put_Line
     ("Size used by the compiler for Small_Shape_Array (Small_Shape_Array'Size):" & Small_Shape_Array'Size'Image);
   Ada.Text_IO.Put_Line
     ("Maximal size computed (Small_Shape_Array_Max_All):"
      & TGen_Marshalling_my_file_Small_Shape_Array_Size_Max_All'Image);
end;

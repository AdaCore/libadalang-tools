with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Pkg;
with Pkg.TGen_Support;
with TGen;
with TGen.TGen_Support;
with Marshaller;

procedure TGen_Marshalling is

   package Integer_Marshaller is new
     Marshaller
       (T             => TGen.TGen_Std.Integer,
        Type_Name     => "Integer",
        Marshal_Value =>
          TGen.TGen_Support.TGen_Marshalling_standard_integer_Input);

   package Answer_Marshaller is new
     Marshaller
       (T             => TGen.TGen_Std.Pkg.Answer,
        Type_Name     => "Answer",
        Marshal_Value => Pkg.TGen_Support.TGen_Marshalling_pkg_answer_Input);

   package Answers_Marshaller is new
     Marshaller
       (T             => TGen.TGen_Std.Pkg.Answer_Array,
        Type_Name     => "Answer_Array",
        Marshal_Value =>
          Pkg.TGen_Support.TGen_Marshalling_pkg_answer_array_Input);

   package User_Answer_Marshaller is new
     Marshaller
       (T             => TGen.TGen_Std.Pkg.User_Answer,
        Type_Name     => "User_Answer",
        Marshal_Value =>
          Pkg.TGen_Support.TGen_Marshalling_pkg_user_answer_Input);

   Integer_Input_Files : constant Integer_Marshaller.File_Array :=
     [To_Unbounded_String
        ("identity_int-5f0dd0e6032219b91371799c4141a5c649ffa986-t1"),
      To_Unbounded_String
        ("identity_int-5f0dd0e6032219b91371799c4141a5c649ffa986-t2"),
      To_Unbounded_String
        ("identity_int-5f0dd0e6032219b91371799c4141a5c649ffa986-t3"),
      To_Unbounded_String
        ("identity_int-5f0dd0e6032219b91371799c4141a5c649ffa986-t4"),
      To_Unbounded_String
        ("identity_int-5f0dd0e6032219b91371799c4141a5c649ffa986-t5")];

   Answer_Input_Files : constant Answer_Marshaller.File_Array :=
     [To_Unbounded_String
        ("identity_answer-9409f060734cea11cf68492ffe4458846003b23f-t1"),
      To_Unbounded_String
        ("identity_answer-9409f060734cea11cf68492ffe4458846003b23f-t2"),
      To_Unbounded_String
        ("identity_answer-9409f060734cea11cf68492ffe4458846003b23f-t3"),
      To_Unbounded_String
        ("identity_answer-9409f060734cea11cf68492ffe4458846003b23f-t4"),
      To_Unbounded_String
        ("identity_answer-9409f060734cea11cf68492ffe4458846003b23f-t5")];

   Answers_Input_Files : constant Answers_Marshaller.File_Array :=
     [To_Unbounded_String
        ("identity_answers-34dc9c3ac61859d6cbfd5cf29819015ba11a7797-t1"),
      To_Unbounded_String
        ("identity_answers-34dc9c3ac61859d6cbfd5cf29819015ba11a7797-t2"),
      To_Unbounded_String
        ("identity_answers-34dc9c3ac61859d6cbfd5cf29819015ba11a7797-t3"),
      To_Unbounded_String
        ("identity_answers-34dc9c3ac61859d6cbfd5cf29819015ba11a7797-t4"),
      To_Unbounded_String
        ("identity_answers-34dc9c3ac61859d6cbfd5cf29819015ba11a7797-t5")];

   User_Answer_Files : constant User_Answer_Marshaller.File_Array :=
     [To_Unbounded_String
        ("identity_user_answer-c17773cd33c94ec4a864305db548649d0d7f23b9-t1"),
      To_Unbounded_String
        ("identity_user_answer-c17773cd33c94ec4a864305db548649d0d7f23b9-t2"),
      To_Unbounded_String
        ("identity_user_answer-c17773cd33c94ec4a864305db548649d0d7f23b9-t3"),
      To_Unbounded_String
        ("identity_user_answer-c17773cd33c94ec4a864305db548649d0d7f23b9-t4")];

begin
   Integer_Marshaller.Log_Values (Integer_Input_Files);
   Answer_Marshaller.Log_Values (Answer_Input_Files);
   Answers_Marshaller.Log_Values (Answers_Input_Files);
   User_Answer_Marshaller.Log_Values (User_Answer_Files);
end TGen_Marshalling;

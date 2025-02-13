package Pkg is
   type Answer is (Yes, No, Unknown);
   type Answer_Array is array (Positive range 1 .. 10) of Answer;

   type User_Answer is record
      Name : String (1 .. 255);
      Age  : Integer;
      Ans  : Answer;
   end record;

   function Identity_Int (X : Integer) return Integer;
   function Identity_Answer (X : Answer) return Answer;
   function Identity_Answers (X : Answer_Array) return Answer_Array;
   function Identity_User_Answer (X : User_Answer) return User_Answer;
end Pkg;

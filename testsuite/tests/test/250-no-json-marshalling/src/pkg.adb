package body Pkg is
   function Identity_Int (X : Integer) return Integer is
   begin
      return X;
   end Identity_Int;

   function Identity_Answer (X : Answer) return Answer is
   begin
      return X;
   end Identity_Answer;

   function Identity_Answers (X : Answer_Array) return Answer_Array is
   begin
      return X;
   end Identity_Answers;

   function Identity_User_Answer (X : User_Answer) return User_Answer is
   begin
      return X;
   end Identity_User_Answer;
end Pkg;

package Examples is

   --  My Functions

   --  My Function F1
   function F1
     (-- 1st EOL comment for V1
      V1 : Integer;
      --  whole line comment for V2
      V2 : Integer;
      --  whole line comment for Str
      Str : String)
      --  Is Length of Str between V1 and V2?
      return Boolean with
     --  Precondition  V1 <= V2;
     Pre => V1 <= V2;

   --  My Function F2
   function F2
     (  -- 1st EOL comment for V1
      V1,
      --  whole line comment for V2
      V2 : Integer;
      --  whole line comment for Str
      Str : String)
      --  Is Length of Str between V1 and V2?
      return Boolean with
     --  Precondition  V1 <= V2;
     Pre => V1 <= V2;

end Examples;

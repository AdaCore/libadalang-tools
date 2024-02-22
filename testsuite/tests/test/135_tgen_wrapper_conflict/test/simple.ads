package Simple is

   procedure Test (Simple : Integer) with
     Pre => Simple >= 1 and then Simple >= 2;

end Simple;

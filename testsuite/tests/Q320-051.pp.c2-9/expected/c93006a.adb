with Report, C93006a1, System;
use Report, C93006a1, System;
procedure C93006a is
begin

   Test
     ("C93006A",
      "CHECK ACTIVATION OF TASK DECLARED IN PACKAGE " & "SPECIFICATION");

   select
      T.E;
   or
      delay 60.0;
      Failed ("RENDEZVOUS NOT ACCEPTED WITHIN 60 SECONDS");
   end select;

   Result;
end C93006a;

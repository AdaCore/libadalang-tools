with Report, C93005g_Pk1;
use Report, C93005g_Pk1;
with System; use System;
procedure C93005g is

begin

   Test ("C93005G", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " & "TASKS");

   Comment ("SUBTEST 5: TASK IN STATEMENT PART OF BLOCK");
   Comment ("  THE TASKS DON'T DEPEND ON THE DECLARATIVE PART");
   B51 : declare
      X : Mnt;
   begin
      B52 : declare
         Y   : Mnt;
         Ptr : Acc_Bad_Rec;
      begin
         Ptr := new Bad_Rec;
         Failed ("EXCEPTION NOT RAISED");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION IN B52");
      end B52;

      Comment ("SUBTEST 5: COMPLETED");
   exception
      when others =>
         Failed ("EXCEPTION NOT ABSORBED");
   end B51;

   Check;

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT ABSORBED");
      Result;
end C93005g;

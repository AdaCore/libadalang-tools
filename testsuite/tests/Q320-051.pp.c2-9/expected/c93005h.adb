with Report, C93005h_Pk1;
use Report, C93005h_Pk1;
with System; use System;
procedure C93005h is

begin

   Test ("C93005H", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " & "TASKS");

   Comment ("SUBTEST 6: TASK IN STATEMENT PART OF PACKAGE");
   Comment ("  THE TASKS DON'T DEPEND ON THE DECLARATIVE PART");
   B61 :
   declare
      X : Mnt;

      package P is
         Y : Mnt;
      end P;

      package body P is
         Ptr : Acc_Bad_Rec;
         Z   : Mnt;
      begin
         Ptr := new Bad_Rec;
         Failed ("EXCEPTION NOT RAISED");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN P");
      end P;

   begin
      Comment ("SUBTEST 6: COMPLETED");
   exception
      when others =>
         Failed ("EXCEPTION NOT ABSORBED");
   end B61;

   Check;

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT ABSORBED");
      Result;
end C93005h;

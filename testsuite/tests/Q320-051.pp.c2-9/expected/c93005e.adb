with Report, C93005e_Pk1;
use Report, C93005e_Pk1;
with System; use System;
procedure C93005e is

begin

   Test ("C93005E", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " & "TASKS");

   Comment ("SUBTEST 3: TASK IN DECL PART OF PACKAGE SPEC");
   Comment ("  THE TASKS DON'T DEPEND ON THE DECLARATIVE PART");
   B31 : declare
      X : Mnt;
   begin
      B32 : begin
         B33 : declare
            package Raises_Exception is
               type Acc_Mnt is access Mnt;
               Y   : Acc_Mnt     := new Mnt;
               Ptr : Acc_Bad_Rec := new Bad_Rec;
            end Raises_Exception;
         begin  -- WOULD HAVE BEEN ACTIVATED HERE
            Failed ("EXCEPTION NOT RAISED");
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG SCOPE");
         end B33;
      exception
         when Constraint_Error =>
            Comment ("SUBTEST 3 COMPLETED");
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN B32");
      end B32;
   end B31;

   Check;

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT ABSORBED");
      Result;
end C93005e;

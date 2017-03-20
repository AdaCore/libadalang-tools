with Report, C93005c_Pk1;
use Report, C93005c_Pk1;
with System; use System;
procedure C93005c is

begin

   Test ("C93005C", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " & "TASKS");

   Comment
     ("SUBTEST 1: TASKS IN DECL PART OF A BLOCK AND A PACKAGE " & "SPEC");
   Comment ("  THE TASKS DEPEND ON THE DECLARATIVE PART");
   B1 : declare
      X : Mnt;
   begin
      B2 : begin
         B3 : declare
            type Acc_Mnt is access Mnt;
            T1 : Unactivated;
            M2 : Acc_Mnt := new Mnt;

            package Raises_Exception is
               T2 : Unactivated;
               M3 : Acc_Mnt  := new Mnt;
               I  : Positive := Ident_Int (0); -- RAISE
               -- CONSTRAINT_ERROR EXCEPTION
            end Raises_Exception;
            use Raises_Exception;
         begin  -- WOULD HAVE BEEN ACTIVATED HERE
            if Equal (I, I) then
               Failed ("EXCEPTION NOT RAISED");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG SCOPE");
         end B3;
      exception
         when Constraint_Error =>
            Comment ("SUBTEST 1 COMPLETED");
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN B2");
      end B2;
   end B1;

   Check;

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT ABSORBED");
      Result;
end C93005c;

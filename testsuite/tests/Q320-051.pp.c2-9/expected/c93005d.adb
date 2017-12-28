with Report, C93005d_Pk1, C93005d_Enqueue;
use Report, C93005d_Pk1;
with System; use System;
procedure C93005d is

begin

   Test ("C93005D", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " & "TASKS");

   Comment
     ("SUBTEST 2: TASKS IN DECL PART OF A BLOCK AND A PACKAGE " & "SPEC");
   Comment ("  THE TASKS DEPEND ON THE DECLARATIVE PART");
   Comment ("  OTHER TASKS HAVE BEEN ENQUEUED ON THE TASKS' ENTRIES");
   B21 :
   declare
      X : Mnt;
   begin
      B22 :
      begin
         B23 :
         declare
            type Acc_Mnt is access Mnt;
            T1 : Unactivated;
            Y  : Acc_Mnt := new Mnt;

            package Has_Unactivated is
               T2 : Unactivated;
               Z  : Acc_Mnt := new Mnt;
               package Enqueue1 is new C93005d_Enqueue (T1);
               package Enqueue2 is new C93005d_Enqueue (T2);
               I : Positive := Ident_Int (0); -- RAISE
               -- CONSTRAINT_ERROR EXCEPTION.
               -- TERMINATES T1 AND T2 AND INDIRECTLY THE 2 T3'S
            end Has_Unactivated;
            use Has_Unactivated;
         begin  -- WOULD HAVE BEEN ACTIVATED HERE
            if Equal (I, I) then
               Failed ("EXCEPTION NOT RAISED");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN WRONG SCOPE");
         end B23;
      exception
         when Constraint_Error =>
            Comment ("SUBTEST 2 COMPLETED");
         when others =>
            Failed ("WRONG EXCEPTION RAISED IN B22");
      end B22;
   end B21;

   Check;

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT ABSORBED");
      Result;
end C93005d;

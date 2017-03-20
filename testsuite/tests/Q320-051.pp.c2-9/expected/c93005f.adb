with Report, C93005f_Pk1;
use Report, C93005f_Pk1;
with System; use System;
procedure C93005f is

begin

   Test ("C93005F", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " & "TASKS");

   Comment ("SUBTEST 4: TASK IN STATEMENT PART OF BLOCK");
   Comment ("  THE TASKS DEPEND ON THE DECLARATIVE PART");
   B41 : declare
      X : Mnt;
   begin
      B42 : declare
         type Local_Acc is access Bad_Rec;
         Y   : Mnt;
         Ptr : Local_Acc;

         type Acc_Mnt is access Mnt;
         Z : Acc_Mnt;

      begin
         Z   := new Mnt;
         Ptr := new Bad_Rec;
         if Ptr.I /= Report.Ident_Int (0) then
            Failed ("EXCEPTION NOT RAISED, VALUE CHANGED");
         else
            Failed ("EXCEPTION NOT RAISED, CONSTRAINT IGNORED");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION IN B42");
      end B42;

      Comment ("SUBTEST 4: COMPLETED");
   exception
      when others =>
         Failed ("EXCEPTION NOT ABSORBED");
   end B41;

   Check;

   Result;

exception
   when others =>
      Failed ("EXCEPTION NOT ABSORBED");
      Result;
end C93005f;

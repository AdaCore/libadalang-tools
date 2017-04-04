with Report; use Report;
with System; use System;
with Shared_C94008d, Counter_C94008d, Events_C94008d;
use Counter_C94008d, Events_C94008d;
procedure C94008d is

   package Trace is new Shared_C94008d (Event_Type, Character, ("....", 0));
   package Terminate_Count is new Shared_C94008d (Integer, Integer, 0);

   procedure Event (Var : Character) renames Trace.Update;

   function Enter_Terminate return Boolean is
   begin
      Terminate_Count.Update (1);
      return True;
   end Enter_Terminate;

begin
   Test
     ("C94008D",
      "CHECK CORRECT OPERATION OF SELECT WITH " &
      "TERMINATE ALTERNATIVE FROM AN INNER BLOCK");

   declare

      task T1 is
         entry E1;
      end T1;

      task body T1 is
      begin
         declare

            task T2 is
               entry E2;
            end T2;

            task body T2 is
            begin
               delay 10.0;

               if Terminate_Count.Get /= 1 then
                  delay 20.0;
               end if;

               if Terminate_Count.Get /= 1 then
                  Failed ("30 SECOND DELAY NOT ENOUGH");
               end if;

               if T1'Terminated or not T1'Callable then
                  Failed ("T1 PREMATURELY TERMINATED");
               end if;

               Event ('A');

               select
                  accept E2;
               or
                  terminate;
               end select;

               Failed ("TERMINATE NOT SELECTED IN T2");
            end T2;

         begin
            begin
               Event ('B');

               select
                  accept E1;
               or when Enter_Terminate =>
                  terminate;
               end select;

               Failed ("TERMINATE NOT SELECTED IN T1");
            end;
         end;
      end T1;

   begin
      Event ('C');
   exception
      when others =>
         Failed ("EXCEPTION RECEIVED IN MAIN");
   end;

   if Trace.Get.Trace (3) = '.' or Trace.Get.Trace (4) /= '.' then
      Failed ("ALL EVENTS NOT PROCESSED CORRECTLY");
   end if;

   Comment ("EXECUTION ORDER WAS " & Trace.Get.Trace);

   Result;
end C94008d;

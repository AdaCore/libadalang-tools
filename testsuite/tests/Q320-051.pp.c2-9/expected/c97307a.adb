-- C97307A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT A TIMED ENTRY CALL THAT IS CANCELED (BECAUSE THE DELAY HAS
-- EXPIRED) IS REMOVED FROM THE QUEUE OF THE CALLED TASK'S ENTRY.

-- WRG 7/14/86

with Report; use Report;
procedure C97307a is

begin

   Test
     ("C97307A",
      "CHECK THAT A TIMED ENTRY CALL THAT IS " &
      "CANCELED (BECAUSE THE DELAY HAS EXPIRED) IS " &
      "REMOVED FROM THE QUEUE OF THE CALLED TASK'S " &
      "ENTRY");

   declare

      Delay_Time : constant Duration := 2 * 60.0;

      task Expired is
         entry Increment;
         entry Read (Count : out Natural);
      end Expired;

      task type Non_Timed_Caller is
         entry Name (N : Natural);
      end Non_Timed_Caller;

      task type Timed_Caller is
         entry Name (N : Natural);
      end Timed_Caller;

      Caller1 : Timed_Caller;
      Caller2 : Non_Timed_Caller;
      Caller3 : Timed_Caller;
      Caller4 : Non_Timed_Caller;
      Caller5 : Timed_Caller;

      task T is
         entry E (Name : Natural);
      end T;

      task Dispatch is
         entry Ready;
      end Dispatch;

      --------------------------------------------------

      task body Expired is
         Expired_Calls : Natural := 0;
      begin
         loop
            select
               accept Increment do
                  Expired_Calls := Expired_Calls + 1;
               end Increment;
            or
               accept Read (Count : out Natural) do
                  Count := Expired_Calls;
               end Read;
            or
               terminate;
            end select;
         end loop;
      end Expired;

      --------------------------------------------------

      task body Non_Timed_Caller is
         My_Name : Natural;
      begin
         accept Name (N : Natural) do
            My_Name := N;
         end Name;

         T.E (My_Name);
      end Non_Timed_Caller;

      --------------------------------------------------

      task body Timed_Caller is
         My_Name : Natural;
      begin
         accept Name (N : Natural) do
            My_Name := N;
         end Name;

         select
            T.E (My_Name);
            Failed
              ("TIMED ENTRY CALL NOT CANCELED FOR CALLER" &
               Natural'Image (My_Name));
         or
            delay Delay_Time;
            Expired.Increment;
         end select;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED IN TIMED_CALLER -- " &
               "CALLER" &
               Natural'Image (My_Name));
      end Timed_Caller;

      --------------------------------------------------

      task body Dispatch is
      begin
         Caller1.Name (1);
         accept Ready;

         Caller2.Name (2);
         accept Ready;

         Caller3.Name (3);
         accept Ready;

         Caller4.Name (4);
         accept Ready;

         Caller5.Name (5);
      end Dispatch;

      --------------------------------------------------

      task body T is

         Desired_Queue_Length : Natural := 1;
         Expired_Calls        : Natural;

         Accepted : array (1 .. 5) of Natural range 0 .. 5 := (others => 0);
         Accepted_Index : Natural                                := 0;

      begin
         loop
            loop
               Expired.Read (Expired_Calls);
               exit when E'Count >= Desired_Queue_Length - Expired_Calls;
               delay 2.0;
            end loop;
            exit when Desired_Queue_Length = 5;
            Dispatch.Ready;
            Desired_Queue_Length := Desired_Queue_Length + 1;
         end loop;

         -- AT THIS POINT, FIVE TASKS WERE QUEUED.
         -- LET THE TIMED ENTRY CALLS ISSUED BY CALLER1,
         -- CALLER3, AND CALLER5 EXPIRE:

         delay Delay_Time + 10.0;

         -- AT THIS POINT, ALL THE TIMED ENTRY CALLS MUST HAVE
         -- EXPIRED AND BEEN REMOVED FROM THE ENTRY QUEUE FOR E,
         -- OTHERWISE THE IMPLEMENTATION HAS FAILED THIS TEST.

         while E'Count > 0 loop
            accept E (Name : Natural) do
               Accepted_Index            := Accepted_Index + 1;
               Accepted (Accepted_Index) := Name;
            end E;
         end loop;

         if Accepted /= (2, 4, 0, 0, 0) then
            Failed ("SOME TIMED CALLS NOT REMOVED FROM ENTRY " & "QUEUE");
            Comment
              ("ORDER ACCEPTED WAS:" &
               Natural'Image (Accepted (1)) &
               ',' &
               Natural'Image (Accepted (2)) &
               ',' &
               Natural'Image (Accepted (3)) &
               ',' &
               Natural'Image (Accepted (4)) &
               ',' &
               Natural'Image (Accepted (5)));
         end if;
      end T;

   --------------------------------------------------

   begin

      null;

   end;

   Result;

end C97307a;

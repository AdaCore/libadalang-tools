-- C99005A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE ATTRIBUTE 'COUNT RETURNS THE CORRECT VALUE.

-- HISTORY:
--     DHH 03/24/88 CREATED ORIGINAL TEST.

with System; use System;
with Report; use Report;
procedure C99005a is

begin

   Test
     ("C99005A",
      "CHECK THAT THE ATTRIBUTE 'COUNT RETURNS THE " & "CORRECT VALUE");

   declare
      task A is
      end A;

      task B is
      end B;

      task C is
      end C;

      task D is
      end D;

      task E is
      end E;

      task F is
      end F;

      task G is
      end G;

      task H is
      end H;

      task I is
      end I;

      task J is
      end J;

      task T is
         entry Wait;
      end T;

      task Choice is
         entry Return_Call;
         entry E2;
         entry E1;
      end Choice;

      task body A is
      begin
         Choice.E1;
      end A;

      task body B is
      begin
         Choice.E1;
      end B;

      task body C is
      begin
         Choice.E1;
      end C;

      task body D is
      begin
         Choice.E1;
      end D;

      task body E is
      begin
         Choice.E1;
      end E;

      task body F is
      begin
         Choice.E2;
      end F;

      task body G is
      begin
         Choice.E2;
      end G;

      task body H is
      begin
         Choice.E2;
      end H;

      task body I is
      begin
         Choice.E2;
      end I;

      task body J is
      begin
         Choice.E2;
      end J;

      task body T is
      begin
         loop
            select
               accept Wait do
                  delay 1.0;
               end Wait;
               Choice.Return_Call;
            or
               terminate;
            end select;
         end loop;
      end T;

      task body Choice is
      begin
         while E1'Count + E2'Count < 10 loop
            T.Wait;
            accept Return_Call;
         end loop;

         for I in reverse 1 .. 10 loop
            select
               accept E2 do
                  if (E2'Count + E1'Count + 1) /= I then
                     Failed
                       ("'COUNT NOT RETURNING " &
                        "CORRECT VALUE FOR LOOP" &
                        Integer'Image (I) &
                        "VALUE " &
                        Integer'Image ((E2'Count + E1'Count + 1)));
                  end if;
               end E2;
            or
               accept E1 do
                  if (E2'Count + E1'Count + 1) /= I then
                     Failed
                       ("'COUNT NOT RETURNING " &
                        "CORRECT VALUE FOR LOOP" &
                        Integer'Image (I) &
                        "VALUE " &
                        Integer'Image ((E2'Count + E1'Count + 1)));
                  end if;
               end E1;
            end select;
         end loop;
      end Choice;

   begin
      null;
   end;

   Result;
end C99005a;

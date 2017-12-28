-- C94008B.ADA

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
-- CHECK THAT A TASK WAITING AT AN OPEN TERMINATE ALTERNATIVE DOES N O T
-- TERMINATE UNTIL ALL OTHER TASKS DEPENDING ON THE SAME UNIT EITHER ARE
-- TERMINATED OR ARE WAITING AT AN OPEN TERMINATE.

-- WEI  3/ 4/82
-- TBN 11/25/85 RENAMED FROM C940BBA-B.ADA.

with Report; use Report;
procedure C94008b is
begin
   Test ("C94008B", "TERMINATION WHILE WAITING AT AN OPEN TERMINATE");

   Block1 :
   declare

      task type Tt1 is
         entry E1;
      end Tt1;

      Numb_Tt1   : constant Natural := 3;
      Delay_Time : Duration         := 0.0;
      Array_Tt1  : array (1 .. Numb_Tt1) of Tt1;

      task body Tt1 is
      begin
         Delay_Time := Delay_Time + 1.0;
         delay Delay_Time;
         for I in 1 .. Numb_Tt1 loop
            if Array_Tt1 (I)'Terminated then
               Failed
                 ("TOO EARLY TERMINATION OF " &
                  "TASK TT1 INDEX" &
                  Integer'Image (I));
            end if;
         end loop;

         select when True =>
            terminate;
         or when False =>
            accept E1;
         end select;
      end Tt1;

   begin  -- BLOCK1.
      for I in 1 .. Numb_Tt1 loop
         if Array_Tt1 (I)'Terminated then
            Failed
              ("TERMINATION BEFORE OUTER " &
               "UNIT HAS BEEN LEFT OF TASK TT1 INDEX " &
               Integer'Image (I));
         end if;
      end loop;
   end Block1;

   Result;

end C94008b;

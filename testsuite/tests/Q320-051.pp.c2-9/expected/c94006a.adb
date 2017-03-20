-- C94006A.ADA

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
-- CHECK THAT A DECLARATION THAT RENAMES A TASK DOES NOT CREATE A NEW
-- MASTER FOR THE TASK.

-- TBN  9/17/86
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94006a is

   task type Tt is
      entry E;
   end Tt;

   task body Tt is
   begin
      select
         accept E;
      or
         delay 30.0;
      end select;
   end Tt;

begin
   Test
     ("C94006A",
      "CHECK THAT A DECLARATION THAT RENAMES A TASK " &
      "DOES NOT CREATE A NEW MASTER FOR THE TASK");

   -------------------------------------------------------------------
   declare
      T1 : Tt;
   begin
      declare
         Rename_Task : Tt renames T1;
      begin
         null;
      end;
      if T1'Terminated then
         Failed ("TASK DEPENDENT ON WRONG UNIT - 1");
      else
         T1.E;
      end if;
   end;

   -------------------------------------------------------------------

   declare
      T2 : Tt;

      package P is
         Q : Tt renames T2;
      end P;

      package body P is
      begin
         null;
      end P;

      use P;
   begin
      if Q'Terminated then
         Failed ("TASK DEPENDENT ON WRONG UNIT - 2");
      else
         Q.E;
      end if;
   end;

   -------------------------------------------------------------------

   declare
      type Acc_Tt is access Tt;
      P1 : Acc_Tt;
   begin
      declare
         Rename_Access : Acc_Tt renames P1;
      begin
         Rename_Access := new Tt;
      end;
      if P1'Terminated then
         Failed ("TASK DEPENDENT ON WRONG UNIT - 3");
      else
         P1.E;
      end if;
   end;

   -------------------------------------------------------------------

   declare
      type Acc_Tt is access Tt;
      P2 : Acc_Tt;

      package Q is
         Rename_Access : Acc_Tt renames P2;
      end Q;

      package body Q is
      begin
         Rename_Access := new Tt;
      end Q;

      use Q;
   begin
      if Rename_Access'Terminated then
         Failed ("TASK DEPENDENT ON WRONG UNIT - 4");
      else
         Rename_Access.E;
      end if;
   end;

   Result;
end C94006a;

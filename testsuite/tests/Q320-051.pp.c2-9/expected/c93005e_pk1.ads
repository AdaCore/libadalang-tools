-- C93005E.ADA

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
-- CHECK THAT IF AN EXCEPTION IS RAISED IN A DECLARATIVE PART OR PACKAGE
-- SPECIFICATION, A TASK DECLARED IN THE SAME DECLARATIVE PART BECOMES
-- COMPLETED BEFORE IT HAS BEEN ACTIVATED; ANY TASKS AWAITING A RENDEZVOUS
-- WITH THE COMPLETED RECEIVE TASKING_ERROR.

-- CASE 3: TASKS IN PACKAGE SPECIFICATION.
--         THE TASKS DON'T DEPEND ON THE PACKAGE SPECIFICATION.

-- RAC 19-MAR-1985
-- JBG 06/03/85
-- EG 10/30/85 ELIMINATE THE USE OF NUMERIC_ERROR IN TEST. PWN 09/11/94 REMOVED
-- PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
pragma Elaborate (Report);
package C93005e_Pk1 is

   -- THIS TYPE OF TASK IS ALWAYS UNACTIVATED.
   task type Unactivated is
      entry E;
   end Unactivated;

   type Acc_Unactivated is access Unactivated;

   type Bad_Rec is record
      T : Unactivated;
      I : Positive := Ident_Int (0); -- RAISE CONSTRAINT_ERROR.
   end record;

   type Acc_Bad_Rec is access Bad_Rec;

   -- ******************************************* DEFINITIONS FOR MUST NOT BE
   -- TERMINATED TASKS *******************************************
   --
   -- THIS SET OF DECLARATIONS DEFINES A RECORD TYPE MNT (MUST NOT TERMINATE).
   -- WHENEVER SUCH A RECORD IS DECLARED, A COUNT IS INCREMENTED AND A TASK IS
   -- CREATED. THE TASK WILL DECREMENT THE COUNT UNLESS IT IS INCORRECTLY AND
   -- PREMATURELY TERMINATED. THE ROUTINE CHECK IS CALLED TO VERIFY WHETHER
   -- THE COUNT HAS RETURNED TO 0 (ALL MNT TASKS GOT A CHANCE TO DO THEIR
   -- DECREMENT).

   -- AN MNT TASK. SUCH TASKS MUST NOT BE TERMINATED BY ANYONE BUT THEMSELVES.
   --
   task type Mnt_Task is
   end Mnt_Task;

   function F return Integer;

   -- THE RECORD THAT IS DECLARED TO HOLD AN MNT TASK AND FORCE CALLING F
   -- BEFORE CREATING THE TASK. F INCREMENTS THE COUNT, THE TASK DECREMENTS
   -- THE COUNT.
   --
   type Mnt is record
      Dummy : Integer := F;
      T     : Mnt_Task;
   end record;

   procedure Check;

   -- ******************************************* END OF DEFINITIONS FOR MUST
   -- NOT BE TERMINATED TASKS *******************************************

end C93005e_Pk1;

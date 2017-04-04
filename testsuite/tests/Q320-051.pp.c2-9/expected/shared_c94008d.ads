-- C94008D.ADA

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
-- CHECK CORRECT OPERATION OF SELECT WITH TERMINATE ALTERNATIVE WHEN EXECUTED
-- FROM AN INNER BLOCK WITH OUTER DEPENDING TASKS.

-- JEAN-PIERRE ROSEN 03-MAR-84 JRK 4/7/86 JBG 9/4/86 ELIMINATED SHARED
-- VARIABLES; ADDED GENERIC UNIT/SUBUNIT PWN 09/11/94 REMOVED PRAGMA
-- PRIORITY FOR ADA 9X.

-- GENERIC UNIT FOR DOING UPDATES OF SHARED VARIABLES
   generic
   type Holder_Type is private;
   type Value_Type is private;
   Initial_Value : Holder_Type;
   with procedure Set (Holder : out Holder_Type; Value : in Holder_Type) is <>;
   with procedure Update
     (Holder : in out Holder_Type;
      Value  : in     Value_Type) is <>;
package Shared_C94008d is
   procedure Set (Value : in Holder_Type);
   procedure Update (Value : in Value_Type);
   function Get return Holder_Type;
end Shared_C94008d;
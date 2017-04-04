-- C86007A.ADA

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
--     CHECK THAT AN EXPANDED NAME FOR AN ENTITY DECLARED IN THE VISIBLE
--     PART OF A LIBRARY PACKAGE CAN START WITH THE NAME "STANDARD".

-- HISTORY:
--     DHH 03/15/88 CREATED ORIGINAL TEST.
--     RJW 10/26/89 ADDED "PRAGMA ELABORATE (REPORT);"

with Report; use Report;
pragma Elaborate (Report);
package C86007a_Pack is
   subtype Item is Integer range 0 .. 10;
   Y : Standard.C86007a_Pack.Item := Ident_Int (5);
   type Acc is access Standard.C86007a_Pack.Item;
   procedure Swap (X, Y : in out Item);
   procedure Proc;
end C86007a_Pack;

-- C86004B1.ADA

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
--     LIBRARY SUBPROGRAM BODY FOR C86004B TEST.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

procedure C86004b1 (Intgr : Integer := Standard.C86004b0 (4)) is

   subtype Int is Integer range 0 .. 10;
   A : Int := Standard.C86004b0 (10);
   B : Int := Standard.C86004b0 (Intgr);

begin
   Test
     ("C86004B",
      "CHECK THAT IF THE SPECIFICATION OF A LIBRARY " &
      "SUBPROGRAM HAS A ""WITH"" CLAUSE FOR A LIBRARY " &
      "SUBPROGRAM M, THEN IN THE FORMAL PART AND IN " &
      "THE BODY (IN ANOTHER FILE), ""STANDARD.M"" IS " &
      "A LEGAL NAME FOR THE SUBPROGRAM M");

   if B /= Standard.C86004b0 (0) then
      Failed ("STANDARD.SUBPROGRAM - B");
   end if;

   if A /= Standard.C86004b0 (10) then
      Failed ("STANDARD.SUBPROGRAM - A");
   end if;

   Result;
end C86004b1;

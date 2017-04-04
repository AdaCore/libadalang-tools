-- C38102C.ADA

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
--     CHECK THAT INCOMPLETE TYPES CAN BE FIXED.

-- HISTORY:
--     DAT 03/24/81  CREATED ORIGINAL TEST.
--     SPS 10/25/82
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  CHANGED VARIOUS
--                   VALUES TO CORRECT CONSTRAINT PROBLEMS.  CHANGED
--                   THE VALUE OF F'DELTA, USING A POWER OF TWO.

with Report; use Report;

procedure C38102c is
begin
   Test ("C38102C", "INCOMPLETE TYPE CAN BE FIXED");

   declare

      type F;
      type G;
      type Af is access F;
      type F is delta 0.25 range -2.0 .. 2.0;
      type G is new F range -1.0 .. 1.5;
      type Ag is access G range -0.75 .. 1.25;

      Xf : Af := new F'(1.0);
      Xg : Ag := new G'(G (Xf.all / 2));

   begin
      if Xg.all not in G then
         Failed ("ACCESS TO FIXED");
      end if;
   end;

   Result;
end C38102c;

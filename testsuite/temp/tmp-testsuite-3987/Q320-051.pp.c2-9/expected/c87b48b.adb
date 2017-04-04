-- C87B48B.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- POSITIONAL ACTUAL PARAMETERS CAN RESOLVE OVERLOADING OF SUBPROGRAMS.

-- TRH  16 AUG 82

with Report; use Report;

procedure C87b48b is

   type Flag is (Pass, Fail);
   type Int is new Integer;
   type Bit is new Boolean;
   type Whl is new Integer range 0 .. Integer'Last;

   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
      type T4 is private;
      Stat : in Flag;
   procedure P1 (W : T1; X : T2; Y : T3; Z : T4);

   procedure P1 (W : T1; X : T2; Y : T3; Z : T4) is
   begin
      if Stat = Fail then
         Failed
           ("RESOLUTION INCORRECT FOR OVERLOADED SUB" &
            "PROGRAMS WITH POSITIONAL ACTUAL PARAMETERS");
      end if;
   end P1;

   procedure P is new P1 (Whl, Int, Whl, Bit, Pass);
   procedure P is new P1 (Whl, Whl, Bit, Int, Fail);
   procedure P is new P1 (Whl, Int, Bit, Whl, Fail);
   procedure P is new P1 (Int, Bit, Whl, Whl, Fail);
   procedure P is new P1 (Bit, Whl, Whl, Int, Fail);
   procedure P is new P1 (Bit, Int, Whl, Whl, Fail);

begin
   Test
     ("C87B48B",
      "OVERLOADING RESOLUTION OF SUBPROGRAMS WITH" &
      " POSITIONAL ACTUAL PARAMETERS");

   begin
      P (0, 0, 0, True);
   end;

   Result;
end C87b48b;

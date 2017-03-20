-- C87B39A.ADA

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
-- CHECK THAT:

--   A) AN OVERLOADED CALL CAN BE RESOLVED BECAUSE AN ALLOCATOR RETURNS
--      AN ACCESS TYPE WHOSE DESIGNATED TYPE IS THE TYPE REFERRED TO IN
--      THE ALLOCATOR.
--
--   B) IF THE NAME OF THE DESIGNATED TYPE IN AN ALLOCATOR DOES NOT
--      UNIQUELY DETERMINE THE ACCESS TYPE OF AN ALLOCATOR, THE CONTEXT
--      MUST DETERMINE THE TYPE.

-- JBG 1/30/84

with Report; use Report;
procedure C87b39a is

   type S is (M, F);
   type R (D : S) is record
      null;
   end record;
   subtype M1 is R (M);
   subtype M2 is R (M);

   type Acc_M1 is access M1;
   type Acc_M2 is access M2;
   type Acc_Bool is access Boolean;
   type Acc_Acc_M1 is access Acc_M1;

   type Which is (Is_M1, Is_M2, Is_Bool);

   procedure P (X : Acc_M1; Resolution : Which) is
   begin
      if Resolution /= Is_M1 then
         Failed ("INCORRECT RESOLUTION -- ACC_M1");
      end if;
   end P;    -- ACC_M1

   procedure P (X : Acc_M2; Resolution : Which) is
   begin
      if Resolution /= Is_M2 then
         Failed ("INCORRECT RESOLUTION -- ACC_M2");
      end if;
   end P;    -- ACC_M2

   procedure P (X : Acc_Bool; Resolution : Which) is
   begin
      if Resolution /= Is_Bool then
         Failed ("INCORRECT RESOLUTION -- ACC_BOOL");
      end if;
   end P;    -- ACC_BOOL

   procedure P (X : Acc_Acc_M1; Resolution : Which) is
   begin
      Failed ("INCORRECT RESOLUTION -- ACC_ACC_M1");
   end P;    -- ACC_ACC_M1

   procedure Q (X : Acc_M1) is
   begin
      null;
   end Q;    -- ACC_M1

   procedure Q (X : Acc_Bool) is
   begin
      Failed ("INCORRECT RESOLUTION -- ACC_BOOL: Q");
   end Q;    -- ACC_BOOL

begin

   Test ("C87B39A", "OVERLOADING RESOLUTION FOR ALLOCATORS");

   P (Acc_M1'(new R (M)), Is_M1);    -- B

   P (Acc_M2'(new M1), Is_M2);      -- B

   P (new Boolean'(True), Is_Bool); -- A

   Q (new M2);              -- A
   Q (new M1);              -- A
   Q (new R (M));            -- A
   Q (new R'(D => M));      -- A

   Result;

end C87b39a;

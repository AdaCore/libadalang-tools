-- C83E02A.ADA

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
-- CHECK THAT WITHIN THE BODY OF A SUBPROGRAM A FORMAL PARAMETER CAN BE
--    USED DIRECTLY IN A RANGE CONSTRAINT, A DISCRIMINANT CONSTRAINT,
--    AND AN INDEX CONSTRAINT.

--    RM    8 JULY 1980

with Report;
procedure C83e02a is

   use Report;

   Z : Integer := 0;

   procedure P1 (A, B : Integer; C : in out Integer) is
      X : Integer range A + 1 .. 1 + B;
   begin
      X := A + 1;
      C := X * B + B * X * A;         -- 4*3+3*4*3=48
   end P1;

   procedure P2 (A, B : Integer; C : in out Integer) is
      type T (Max : Integer) is record
         Value : Integer range 1 .. 3;
      end record;
      X : T (A);
   begin
      X := (Max => 4, Value => B); -- ( 4 , 3 )
      C := 10 * C + X.Value + 2;        -- 10*48+3+2=485
   end P2;

   function F3 (A, B : Integer) return Integer is
      type Table is array (A .. B) of Integer;
      X : Table;
      Y : array (A .. B) of Integer;
   begin
      X (A) := A;                      -- 5
      Y (B) := B;                      -- 6
      return X (A) - Y (B) + 4;             -- 3
   end F3;

begin

   Test
     ("C83E02A",
      "CHECK THAT WITHIN THE BODY OF A SUBPROGRAM " &
      " A FORMAL PARAMETER CAN BE USED DIRECTLY IN" &
      " A RANGE CONSTRAINT, A DISCRIMINANT CONSTRAINT" &
      ", AND AN INDEX CONSTRAINT");

   P1 (3, 3, Z);                     --  Z  BECOMES  48
   P2 (4, F3 (5, 6), Z);           --  Z  BECOMES 485

   if Z /= 485 then
      Failed ("ACCESSING ERROR OR COMPUTATION ERROR");
   end if;

   Result;

end C83e02a;

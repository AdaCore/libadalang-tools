-- C74406A.ADA

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
--     CHECK THAT THE FULL DECLARATION OF A LIMITED PRIVATE TYPE CAN
--     DECLARE A TASK TYPE, A TYPE DERIVED FROM A LIMITED PRIVATE TYPE,
--     AND A COMPOSITE TYPE WITH A COMPONENT OF A LIMITED TYPE.

-- HISTORY:
--     BCB 03/10/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C74406a is

   package Tp is
      type T is limited private;
      procedure Init (Z1 : out T; Z2 : Integer);
      function Equal_T (One, Two : T) return Boolean;
   private
      type T is range 1 .. 100;
   end Tp;

   package body Tp is
      procedure Init (Z1 : out T; Z2 : Integer) is
      begin
         Z1 := T (Z2);
      end Init;

      function Equal_T (One, Two : T) return Boolean is
      begin
         if Equal (3, 3) then
            return One = Two;
         else
            return One /= Two;
         end if;
      end Equal_T;
   begin
      null;
   end Tp;

   use Tp;

   package P is
      type T1 is limited private;
      type T2 is limited private;
      type T3 is limited private;
      type T4 is limited private;
   private
      task type T1 is
         entry Here (Val1 : in out Integer);
      end T1;

      type T2 is new T;

      type T3 is record
         Int : T;
      end record;

      type T4 is array (1 .. 5) of T;
   end P;

   package body P is
      X1  : T1;
      X3  : T3;
      X4  : T4;
      Var : Integer := 25;

      task body T1 is
      begin
         accept Here (Val1 : in out Integer) do
            Val1 := Val1 * 2;
         end Here;
      end T1;

   begin
      Test
        ("C74406A",
         "CHECK THAT THE FULL DECLARATION OF A " &
         "LIMITED PRIVATE TYPE CAN DECLARE A TASK " &
         "TYPE, A TYPE DERIVED FROM A LIMITED " &
         "PRIVATE TYPE, AND A COMPOSITE TYPE WITH " &
         "A COMPONENT OF A LIMITED TYPE");

      X1.Here (Var);

      if not Equal (Var, Ident_Int (50)) then
         Failed ("IMPROPER VALUE FOR VAL");
      end if;

      Init (X3.Int, 50);

      if X3.Int not in T then
         Failed ("IMPROPER RESULT FROM MEMBERSHIP TEST");
      end if;

      Init (X4 (3), 17);

      if not Equal_T (T'(X4 (3)), T (X4 (3))) then
         Failed
           ("IMPROPER RESULT FROM QUALIFICATION AND " & "EXPLICIT CONVERSION");
      end if;

      Result;
   end P;

   use P;

begin
   null;
end C74406a;

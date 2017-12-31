-- C58005A.ADA

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
-- CHECK THAT WHEN A FUNCTION IS READY TO RETURN CONTROL TO ITS INVOKER
--    THE CONSTRAINTS ON THE RETURN VALUES ARE CHECKED, AND THAT
--     CONSTRAINT ERROR  IS THEN RAISED IF AND ONLY IF THE CONSTRAINTS
--    ARE NOT SATISFIED.

-- THIS TEST CHECKS THAT THE EXCEPTION IS RAISED UNDER THE APPROPRIATE
--    CONDITIONS; IT ALSO CHECKS THE IDENTITY OF THE EXCEPTION.  THE
--    PRECISE MOMENT AND PLACE THE EXCEPTION IS RAISED IS TESTED
--    ELSEWHERE.

-- RM 05/14/81
-- SPS 10/26/82

with Report;
procedure C58005a is

   use Report;

   Intvar : Integer;

begin

   Test
     ("C58005A",
      "CHECK THAT EXCEPTIONS ARE RAISED BY A RETURN" &
      " STATEMENT IF AND ONLY IF THE CONSTRAINTS ARE" & " VIOLATED");

   declare
      subtype I1 is Integer range -10 .. 90;
      subtype I2 is Integer range 1 .. 10;
      function Fn1 (X : I1) return I2 is
      begin
         return 0;
      end Fn1;

      function Fn2 (X : I1) return I2 is
      begin
         return X + Ident_Int (0);
      end Fn2;

      function Fn3 (X : I1) return I2 is
         Hundred : Integer range -100 .. 100 := Ident_Int (100);
      begin
         return Hundred - 90;
      end Fn3;

   begin

      Intvar := 0;

      begin
         Intvar := Fn1 (0) + Intvar;  -- EXCEPTION.
         Failed ("EXCEPTION NOT RAISED  -  1");
      exception
         when Constraint_Error =>
            Intvar := Intvar + 10;
         when others =>
            Failed ("WRONG EXCEPTION RAISED  -  1");
      end;

      begin
         Intvar := Fn2 (1) + Intvar; -- 10+1=11 -- NO EXCEPTION.
         Intvar := Intvar + 100;   -- 11+100=111
      exception
         when others =>
            Failed ("EXCEPTION RAISED  -  2");
      end;

      begin
         Intvar := Fn2 (11) + Intvar;  -- EXCEPTION.
         Failed ("EXCEPTION NOT RAISED  -  3");
      exception
         when Constraint_Error =>
            Intvar := Intvar + 10; -- 121
         when others =>
            Failed ("WRONG EXCEPTION RAISED  -  3");
      end;

      begin
         Intvar := Fn3 (0) + Intvar;--121+10=131 --NO EXCEPTION.
         Intvar := Intvar + 1_000;-- 131+1000=1131
      exception
         when others =>
            Failed ("EXCEPTION RAISED  -  4");
      end;

   end;

   if Intvar /= 1_131 then
      Failed ("WRONG FLOW OF CONTROL");
   end if;

   Result;

end C58005a;

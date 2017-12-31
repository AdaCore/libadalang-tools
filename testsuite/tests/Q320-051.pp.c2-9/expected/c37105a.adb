-- C37105A.ADA

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
-- CHECK THAT RECORDS WITH ONLY DISCRIMINANTS ARE OK.

-- DAT 5/18/81
-- JWC 6/28/85 RENAMED TO -AB

with Report; use Report;

procedure C37105a is
begin
   Test ("C37105A", "RECORDS WITH ONLY DISCRIMINANTS");

   declare
      type R1 (D : Boolean) is record
         null;
      end record;
      type R2 (D, E : Boolean) is record
         null;
      end record;
      type R3 (A, B, C, D : Integer; W, X, Y, Z : Character) is record
         null;
      end record;
      Obj1 : R1 (Ident_Bool (True));
      Obj2 : R2 (Ident_Bool (False), Ident_Bool (True));
      Obj3 : R3 (1, 2, 3, 4, 'A', 'B', 'C', Ident_Char ('D'));
   begin
      if Obj1 = (D => (False)) or Obj2 /= (False, (True)) or
        Obj3 /= (1, 2, 3, 4, 'A', 'B', 'C', ('D')) then
         Failed ("DISCRIMINANT-ONLY RECORDS DON'T WORK");
      end if;
   end;

   Result;
end C37105a;

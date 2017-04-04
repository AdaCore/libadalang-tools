-- C45672A.ADA

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
-- CHECK THAT "NOT" YIELDS THE CORRECT RESULTS WHEN APPLIED TO ONE-DIMENSIONAL
-- BOOLEAN ARRAYS.

-- JWC 11/15/85

with Report; use Report;

procedure C45672a is
begin

   Test
     ("C45672A",
      "CHECK THE UNARY OPERATOR 'NOT' APPLIED TO " &
      "ONE-DIMENSIONAL BOOLEAN ARRAYS");

   declare

      type Arr1 is array (Integer range 1 .. 4) of Boolean;
      type Arr2 is array (Integer range 1 .. 40) of Boolean;
      type Arr3 is array (Integer range <>) of Boolean;
      type Arr4 is array (Integer range 1 .. 4) of Boolean;
      type Arr5 is array (Integer range 1 .. 40) of Boolean;

      pragma Pack (Arr4);
      pragma Pack (Arr5);

      A1 : Arr1 := Arr1'(1 | 3 => True, others => False);
      A2 : Arr2 :=
        Arr2'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => True, others => False);
      A3 : Arr3 (Ident_Int (3) .. Ident_Int (4)) := Arr3'(True, False);
      A4 : Arr4 := Arr4'(1 | 3 => True, others => False);
      A5 : Arr5                                  :=
        Arr5'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => True, others => False);
      A6 : Arr3 (Ident_Int (9) .. Ident_Int (7));

      procedure P (A : Arr3; F : Integer; L : Integer) is
      begin
         if A'First /= F or A'Last /= L then
            Failed ("'NOT' YIELDED THE WRONG BOUNDS");
         end if;
      end P;

   begin

      P (not A3, 3, 4);
      P (not A6, 9, 7);

      if not A1 /= Arr1'(1 | 3 => False, others => True) then
         Failed ("WRONG RESULT WHEN 'NOT' APPLIED " & "TO SMALL ARRAY");
      end if;

      if not A2 /=
        Arr2'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => False, others => True)
      then
         Failed ("WRONG RESULT WHEN 'NOT' APPLIED " & "TO LARGE ARRAY");
      end if;

      if not A4 /= Arr4'(1 | 3 => False, others => True) then
         Failed ("WRONG RESULT WHEN 'NOT' APPLIED " & "TO SMALL PACKED ARRAY");
      end if;

      if not A5 /=
        Arr5'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => False, others => True)
      then
         Failed ("WRONG RESULT WHEN 'NOT' APPLIED " & "TO LARGE PACKED ARRAY");
      end if;

      if "NOT" (Right => A1) /= Arr1'(1 | 3 => False, others => True) then
         Failed
           ("WRONG RESULT WHEN 'NOT' APPLIED " &
            "TO SMALL ARRAY USING NAMED NOTATION");
      end if;

      if "NOT" (Right => A5) /=
        Arr5'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => False, others => True)
      then
         Failed
           ("WRONG RESULT WHEN 'NOT' APPLIED TO LARGE " &
            "PACKED ARRAY USING NAMED NOTATION");
      end if;

   end;

   Result;

end C45672a;

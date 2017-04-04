-- C45347B.ADA

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
-- CHECK THAT CATENATION IS DEFINED FOR ARRAY TYPES AS COMPONENT TYPES.

-- JWC 11/15/85

with Report; use Report;

procedure C45347b is

begin

   Test
     ("C45347B",
      "CHECK THAT CATENATION IS DEFINED " &
      "FOR ARRAY TYPES AS COMPONENT TYPES");

   declare

      type Arr is array (1 .. 2) of Integer;
      type A is array (Integer range <>) of Arr;

      Ar1 : Arr := (4, 1);
      Ar2 : Arr := (1, 1);

      A1 : A (1 .. 2) := ((1, 1), (2, 1));
      A2 : A (1 .. 2) := ((3, 1), (4, 1));
      A3 : A (1 .. 4) := ((1, 1), (2, 1), (3, 1), (4, 1));
      A4 : A (1 .. 4);
      A5 : A (1 .. 4) := ((4, 1), (3, 1), (2, 1), (1, 1));

   begin

      A4 := A1 & A2;

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ARRAYS OF ARRAYS");
      end if;

      A4 := A5;

      A4 := A1 & A2 (1) & Ar1;

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ARRAY OF ARRAYS " & "WITH ARRAYS");
      end if;

      A4 := A5;

      A4 := Ar2 & (A1 (2) & A2);

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ARRAYS WITH ARRAYS " & "OF ARRAYS");
      end if;

      A4 := A5;

      A4 := A'(Ar2 & A1 (2)) & A'(A2 (1) & Ar1);

      if A3 /= A4 then
         Failed ("INCORRECT CATENATION FOR ARRAYS");
      end if;

   end;

   Result;

end C45347b;

-- C45262C.ADA

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
-- CHECK THAT ORDERING COMPARISONS YIELD CORRECT RESULTS FOR ONE-DIMENSIONAL
-- DISCRETE ARRAY TYPES. THIS TEST CHECKS ARRAYS OF AN ENUMERATION TYPE.

-- JWC 8/19/85
-- JRK 6/24/86 FIXED SPELLING IN FAILURE MESSAGE.

with Report; use Report;

procedure C45262c is
begin
   Test
     ("C45262C",
      "ORDERING COMPARISONS OF ONE-DIMENSIONAL " &
      "DISCRETE ARRAY TYPES - ENUMERATED COMPONENTS");

   declare

      subtype Subint is Integer range 0 .. 5;
      type Enum is (E0, E1);
      type Arr is array (Subint range <>) of Enum;
      Arr1 : Arr (1 .. Ident_Int (0));
      Arr2 : Arr (2 .. Ident_Int (0));
      Arr3 : Arr (1 .. Ident_Int (1)) := (Ident_Int (1) => E0);
      Arr4 : Arr (0 .. Ident_Int (0)) := (Ident_Int (0) => E0);
      Arr5 : Arr (0 .. Ident_Int (0)) := (Ident_Int (0) => E1);
      Arr6 : Arr (1 .. Ident_Int (5)) := (1 .. Ident_Int (5) => E0);
      Arr7 : Arr (0 .. 4)             := (0 .. 3 => E0, 4 => E1);
      Arr8 : Arr (0 .. Ident_Int (4)) := (0 .. Ident_Int (4) => E0);
      Arr9 : Arr (0 .. Ident_Int (3)) := (0 .. Ident_Int (3) => E0);
      Arra : Arr (0 .. Ident_Int (3)) := (0 .. Ident_Int (3) => E1);

   begin
      if Arr1 < Arr2 then
         Failed ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - <");
      end if;

      if not (Arr1 <= Arr2) then
         Failed ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - <=");
      end if;

      if Arr1 > Arr2 then
         Failed ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - >");
      end if;

      if not (">=" (Arr1, Arr2)) then
         Failed ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - >=");
      end if;

      if Arr3 < Arr1 then
         Failed ("NON-NULL ARRAY ARR3 LESS THAN NULL ARR1");
      end if;

      if Arr3 <= Arr1 then
         Failed ("NON-NULL ARRAY ARR3 LESS THAN EQUAL NULL ARR1");
      end if;

      if not (">" (Arr3, Arr1)) then
         Failed ("NON-NULL ARRAY ARR3 NOT GREATER THAN NULL " & "ARR1");
      end if;

      if not (Arr3 >= Arr1) then
         Failed ("NON-NULL ARRAY ARR3 NOT GREATER THAN EQUAL " & "NULL ARR1");
      end if;

      if Arr3 < Arr4 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - <");
      end if;

      if not ("<=" (Arr3, Arr4)) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - <=");
      end if;

      if Arr3 > Arr4 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - >");
      end if;

      if not (Arr3 >= Arr4) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - >=");
      end if;

      if not ("<" (Arr3, Arr5)) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - <");
      end if;

      if not (Arr3 <= Arr5) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - <=");
      end if;

      if Arr3 > Arr5 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - >");
      end if;

      if Arr3 >= Arr5 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - >=");
      end if;

      if not (Arr6 < Arr7) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - <");
      end if;

      if not (Arr6 <= Arr7) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " & "<=");
      end if;

      if Arr6 > Arr7 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - >");
      end if;

      if ">=" (Left => Arr6, Right => Arr7) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " & ">=");
      end if;

      if Arr6 < Arr8 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <");
      end if;

      if not (Arr6 <= Arr8) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <=");
      end if;

      if ">" (Right => Arr8, Left => Arr6) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >");
      end if;

      if not (Arr6 >= Arr8) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >=");
      end if;

      if Arr8 < Arr9 then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - <");
      end if;

      if Arr8 <= Arr9 then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - <=");
      end if;

      if not (Arr8 > Arr9) then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - >");
      end if;

      if not (Arr8 >= Arr9) then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - >=");
      end if;

      if not (Arr8 < Arra) then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - <");
      end if;

      if not (Arr8 <= Arra) then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - <=");
      end if;

      if Arr8 > Arra then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - >");
      end if;

      if Arr8 >= Arra then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - >=");
      end if;

   end;

   Result;

end C45262c;

-- C45262B.ADA

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
-- DISCRETE ARRAY TYPES. THIS TEST CHECKS STRING TYPES.

-- JWC 9/9/85
-- JRK 6/24/86 FIXED SPELLING IN FAILURE MESSAGE.

with Report; use Report;

procedure C45262b is
begin
   Test
     ("C45262B",
      "ORDERING COMPARISONS OF ONE-DIMENSIONAL " &
      "DISCRETE ARRAY TYPES - TYPE STRING");

   declare

      String1 : String (2 .. Ident_Int (1));
      String2 : String (3 .. Ident_Int (1));
      String3 : String (2 .. Ident_Int (2)) := (Ident_Int (2) => 'A');
      String4 : String (1 .. Ident_Int (1)) := (Ident_Int (1) => 'A');
      String5 : String (1 .. Ident_Int (1)) := (Ident_Int (1) => 'B');
      String6 : String (2 .. Ident_Int (6)) := (2 .. Ident_Int (6) => 'A');
      String7 : String (1 .. 5)             := (1 .. 4 => 'A', 5 => 'B');
      String8 : String (1 .. Ident_Int (5)) := (1 .. Ident_Int (5) => 'A');
      String9 : String (1 .. Ident_Int (4)) := (1 .. Ident_Int (4) => 'A');
      Stringa : String (1 .. Ident_Int (4)) := (1 .. Ident_Int (4) => 'B');

   begin
      if String1 < String2 then
         Failed ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - <");
      end if;

      if not (String1 <= String2) then
         Failed ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - " & "<=");
      end if;

      if String1 > String2 then
         Failed ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - >");
      end if;

      if not (">=" (String1, String2)) then
         Failed ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - " & ">=");
      end if;

      if String3 < String1 then
         Failed ("NON-NULL ARRAY STRING3 LESS THAN NULL STRING1");
      end if;

      if String3 <= String1 then
         Failed ("NON-NULL ARRAY STRING3 LESS THAN EQUAL NULL " & "STRING1");
      end if;

      if not (">" (String3, String1)) then
         Failed ("NON-NULL ARRAY STRING3 NOT GREATER THAN NULL " & "STRING1");
      end if;

      if not (String3 >= String1) then
         Failed
           ("NON-NULL ARRAY STRING3 NOT GREATER THAN " & "EQUAL NULL STRING1");
      end if;

      if String3 < String4 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - <");
      end if;

      if not ("<=" (String3, String4)) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - <=");
      end if;

      if String3 > String4 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - >");
      end if;

      if not (String3 >= String4) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS EQUAL - >=");
      end if;

      if not ("<" (String3, String5)) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - <");
      end if;

      if not (String3 <= String5) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - <=");
      end if;

      if String3 > String5 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - >");
      end if;

      if String3 >= String5 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "COMPONENTS NOT EQUAL - >=");
      end if;

      if not (String6 < String7) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - <");
      end if;

      if not (String6 <= String7) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " &
            "<=");
      end if;

      if String6 > String7 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - >");
      end if;

      if ">=" (Left => String6, Right => String7) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " &
            ">=");
      end if;

      if String6 < String8 then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <");
      end if;

      if not (String6 <= String8) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <=");
      end if;

      if ">" (Right => String8, Left => String6) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >");
      end if;

      if not (String6 >= String8) then
         Failed
           ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
            "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >=");
      end if;

      if String8 < String9 then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - <");
      end if;

      if String8 <= String9 then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - <=");
      end if;

      if not (String8 > String9) then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - >");
      end if;

      if not (String8 >= String9) then
         Failed ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS EQUAL - >=");
      end if;

      if not (String8 < Stringa) then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - <");
      end if;

      if not (String8 <= Stringa) then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - <=");
      end if;

      if String8 > Stringa then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - >");
      end if;

      if String8 >= Stringa then
         Failed
           ("DIFFERENT NUMBER OF COMPONENTS, " & "COMPONENTS NOT EQUAL - >=");
      end if;

   end;

   Result;

end C45262b;

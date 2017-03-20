-- C57003A.ADA

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
-- CHECK THAT THE EXIT STATEMENT IS EVALUATED EACH TIME THROUGH A LOOP,
--    AND THAT IT IS EVALUATED CORRECTLY WHETHER POSITIONED AT THE
--    BEGINNING, MIDDLE, OR END OF THE LOOP.

-- EACH TEST IS A LOOP ON  J  WHERE THE EXIT CONDITIONS ARE TO EVALUATE
--    TO  'FALSE'  A CERTAIN NUMBER OF TIMES UNTIL, AT THE APPROPRIATE
--    TIME, ONE OF THEM EVALUATES TO  'TRUE'  AND CAUSES THE LOOP TO BE
--    EXITED.
--
--
--    THE TEST IS PERFORMED 30 TIMES FOR EACH OF THE FIRST TWO
--    DATA TYPES CONSIDERED ('INTEGER', USER-DEFINED ENUMERATION)
--    AND 26 TIMES FOR 'CHARACTER' (THUS 86 TIMES ALTOGETHER).
--
--
--    EACH DATA TYPE HAS ITS OWN SEPARATE SECTION OF CODE.  ALL SECTIONS
--    FOLLOW THE SAME TESTING ALGORITHM (MUTATIS MUTANDIS).  THE CALCU-
--    LATIONS WHICH KEEP TRACK OF THE FLOW OF CONTROL ARE ALL DONE IN
--    INTEGER ARITHMETIC.  THERE ARE THREE DATA TYPES, THUS THREE
--    SECTIONS.
--
--
--    FOR EACH DATA TYPE, THE 30 TESTS ARE DIVIDED INTO 3 "SEGMENTS"
--
--               << NOTE:  THE NUMBER OF SEGMENTS IS WRITTEN  " 3 "    ,
--                         THE NUMBER OF SECTIONS IS WRITTEN  "THREE" >>
--
--    (OF 10 TESTS EACH, EXCEPT 10,10,6 FOR 'CHARACTER'), NUMBERED
--     0 , 1 , 2  AND CORRESPONDING TO THE 3 SIGNIFICANTLY DIFFERENT
--    POSITIONS OF AN EXIT STATEMENT WITH RESPECT TO THE LOOP IT IS IN
--    ( "AT THE VERY TOP" , "AT THE VERY BOTTOM" , "ANYWHERE IN BETWEEN"
--    ).  AT THE BEGINNING OF EACH TEST, THE VARIABLE  WHICH_SEGMENT
--    IS UPDATED TO CONTAIN THE NEW VALUE OF THIS IDENTIFYING NUMBER
--    (FOR THE TEST ABOUT TO BEGIN):
--
--             EXIT AT THE TOP      ........   WHICH_SEGMENT = 0
--             EXIT FROM THE MIDDLE ........   WHICH_SEGMENT = 1
--             EXIT AT THE BOTTOM   ........   WHICH_SEGMENT = 2   .
--
--
--    WITHIN EACH SECTION, THE TESTS ARE NUMBERED  FROM  1  TO  30
--    (26 FOR 'CHARACTER').  THIS NUMBER IS STORED IN THE INTEGER
--    VARIABLE  INT_I  (EQUAL TO THE CURRENT VALUE OF THE OUTER-LOOP
--    INDEX WHEN THAT INDEX IS OF INTEGER TYPE), WHOSE APPROPRIATE VALUE
--    FOR EACH TEST IS SET AT THE BEGINNING OF THE TEST.
--
--
--    AS PART OF THE EVALUATION PROCESS, THE PROGRAM COMPUTES FOR EACH
--    TEST (I.E.  FOR EACH VALUE OF  I , OR OF  INT_I ) THE APPROPRIATE
--    NUMBER OF INNER-LOOP ITERATIONS REQUIRED BEFORE EXIT; THIS IS
--    THE EXPECTED VALUE OF  J  (EXPRESSED AS AN INTEGER IN THE RANGE
--     1..10 ) AND STORES IT IN  EXPECTED_J .  FOR EACH OF THE THREE
--    SECTIONS, THE TIME SEQUENCE OF THESE 30 VALUES IS
--
--             1   2   3   4   5   6   7   8   9  10     << SEGMENT 1 >>
--             6   6   7   7   8   8   9   9  10  10     << SEGMENT 2 >>
--             7   8   8   8   9   9   9  10  10  10     << SEGMENT 3 >>
--
--    (EACH SECTION GETS ALL 3 ROWS, NOT ONE ROW PER SECTION;
--    FOR 'CHARACTER', WHERE ONLY 26 VALUES ARE REQUIRED, THE LAST 4
--    VALUES ARE OMITTED).  THIS NUMBER IS COMPARED WITH THE ACTUAL
--    VALUE OF  J  (ACTUAL NUMBER OF INNER-LOOP ITERATIONS BEFORE THE
--    EXECUTION OF THE EXIT STATEMENT) AS SAVED JUST BEFORE THE EXIT
--    FROM THE LOOP (AGAIN IN THE FORM OF AN INTEGER IN THE RANGE
--     1..30 , IRRESPECTIVE OF THE DATA TYPE BEING TESTED),  I F
--    SUCH SAVED VALUE IS AVAILABLE.
--
--
--    THE ACTUAL VALUE OF INNER-LOOP ITERATIONS (AS SAVED IMMEDIATELY
--    BEFORE THE EXIT, AS OPPOSED TO A VALUE LEFT OVER FROM SOME
--    PREVIOUS ITERATION) IS AVAILABLE ONLY IF  WHICH_SEGMENT /= 0 ,
--    AND IS THEN STORED IN  SAVE_J .
--
--
--    FOR THE CASE  WHICH_SEGMENT = 0 , THE ITERATIONS ARE COUNTED IN
--    THE VARIABLE  COUNT , WHOSE VALUE AT THE COMPLETION OF THE
--    I-TH TEST ( I IN 1..10 ) MUST BE EQUAL TO  EXPECTED_J - 1 ,
--    AND THUS TO  I - 1  (METHODOLOGICALLY AS WELL AS COMPUTATIONALLY
--    THIS IS NO DIFFERENT FROM USING THE MOST RECENT VALUE OF  SAVE_J
--    WHEN A CURRENT ONE CANNOT BE OBTAINED).  AFTER BEING INCREMENTED
--    BY  1 ,  COUNT  IS CHECKED AGAINST  EXPECTED_J .
--
--
--    THIS CONCLUDES THE DESCRIPTION OF THE CASE  WHICH_SEGMENT = 0 ,
--    AND THUS OF THE ALGORITHM.  THE ONLY REASON FOR SPLITTING THE
--    CASE  WHICH_SEGMENT /= 0  INTO TWO IS THE DESIRE TO PROVIDE FOR
--    DISTINCT MESSAGES.

-- RM 04/23/81
-- SPS 3/7/83

with Report;
procedure C57003a is

   use Report;

begin

   Test
     ("C57003A",
      "TEST THAT THE EXIT STATEMENT IS EVALUATED" &
      " EACH TIME THROUGH THE LOOP");

   declare

      Which_Segment : Integer range 0 .. 2;   -- BOUNDS ARE TIGHT
      Save_J        : Integer range 1 .. 10;
      Expected_J    : Integer range 1 .. 10;
      Count         : Integer range 0 .. 100 := 0;
      Int_I         : Integer range 1 .. 30;

      type Enum is
        (Change_The_Origin_From_0_To_1,

         A1,
         A2,
         A3,
         A4,
         A5,
         A6,
         A7,
         A8,
         A9,
         A10,
         A11,
         A12,
         A13,
         A14,
         A15,
         A16,
         A17,
         A18,
         A19,
         A20,
         A21,
         A22,
         A23,
         A24,
         A25,
         A26,
         A27,
         A28,
         A29,
         A30);

   begin

      --------------------------------------------------------------
      -----------------------  INTEGER  ----------------------------

      for I in Integer range 1 .. 30 loop

         Which_Segment := (I - 1) / 10;
         Expected_J    := (I + Which_Segment) / (Which_Segment + 1);

         Count := 0;

         for J in Integer range 1 .. 10 loop

            --  J  NOT SAVED HERE (SO THAT 'EXIT' BE FIRST STMT)

            exit when Which_Segment = 0 and
              1 * J >= I;--COUNT+:=1 ON NXT LINE INSTEAD
            Count := Count + 1;

            null;
            null;
            null;
            Save_J := J;
            exit when Which_Segment = 1 and 2 * J >= I;

            null;
            null;
            null;
            Save_J := J;
            exit when Which_Segment = 2 and 3 * J >= I;

         end loop;

         Count := Count + 1;  -- SEE HEADER

         case Which_Segment is
            when 0 =>
               if Count /= Expected_J then
                  Failed ("WRONG COUNT; INT, EXIT AT TOP");
               end if;
            when 1 =>                 -- WOULD WORK ALSO FOR 0
               if Save_J /= Expected_J then
                  Failed ("WRONG COUNT; I,EXIT AT MIDDLE");
               end if;
            when 2 =>
               if Save_J /= Expected_J then
                  Failed ("WRONG COUNT; I,EXIT AT BOTTOM");
               end if;
         end case;

      end loop;

      --------------------------------------------------------------
      ----------------------  CHARACTER  ---------------------------

      for I in Character range 'A' .. 'Z' loop

         Int_I := Character'Pos (I) - Character'Pos ('A') + 1;

         Which_Segment := (Int_I - 1) / 10;
         Expected_J    := (Int_I + Which_Segment) / (Which_Segment + 1);

         Count := 0;

         for J in Character range 'A' .. 'J' loop

            --  J  NOT SAVED HERE (SO THAT 'EXIT' BE FIRST STMT)

            exit when Which_Segment = 0 and
              J >= I; -- COUNT+:=1 ON NXT LINE INSTEAD
            Count := Count + 1;

            null;
            null;
            null;
            Save_J := Character'Pos (J) - Character'Pos ('A') + 1;
            exit when Which_Segment = 1 and 2 * Save_J >= Int_I;

            null;
            null;
            null;
            exit when Which_Segment = 2 and 3 * Save_J >= Int_I;

         end loop;

         Count := Count + 1;

         case Which_Segment is
            when 0 =>
               if Count /= Expected_J then
                  Failed ("WRONG COUNT;CHAR, EXIT AT TOP");
               end if;
            when 1 =>                 -- WOULD WORK ALSO FOR 0
               if Save_J /= Expected_J then
                  Failed ("WRONG COUNT; C,EXIT AT MIDDLE");
               end if;
            when 2 =>
               if Save_J /= Expected_J then
                  Failed ("WRONG COUNT; C,EXIT AT BOTTOM");
               end if;
         end case;

      end loop;

      --------------------------------------------------------------
      ---------------------  ENUMERATION  --------------------------

      for I in Enum range A1 .. A30 loop

         Int_I := Enum'Pos (I);

         Which_Segment := (Int_I - 1) / 10;
         Expected_J    := (Int_I + Which_Segment) / (Which_Segment + 1);

         Count := 0;

         for J in Enum range A1 .. A10 loop

            --  J  NOT SAVED HERE (SO THAT 'EXIT' BE FIRST STMT)

            exit when Which_Segment = 0 and
              J >= I; -- COUNT+:=1 ON NXT LINE INSTEAD
            Count := Count + 1;

            null;
            null;
            null;
            Save_J := Enum'Pos (J);
            exit when Which_Segment = 1 and 2 * Save_J >= Int_I;

            null;
            null;
            null;
            exit when Which_Segment = 2 and 3 * Save_J >= Int_I;

         end loop;

         Count := Count + 1;

         case Which_Segment is
            when 0 =>
               if Count /= Expected_J then
                  Failed ("WRONG COUNT;ENUM, EXIT AT TOP");
               end if;
            when 1 =>                 -- WOULD WORK ALSO FOR 0
               if Save_J /= Expected_J then
                  Failed ("WRONG COUNT; E,EXIT AT MIDDLE");
               end if;
            when 2 =>
               if Save_J /= Expected_J then
                  Failed ("WRONG COUNT; E,EXIT AT BOTTOM");
               end if;
         end case;

      end loop;

      --------------------------------------------------------------

   end;

   Result;

end C57003a;

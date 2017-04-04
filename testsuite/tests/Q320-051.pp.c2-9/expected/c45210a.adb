-- C45210A.ADA

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
-- CHECK THAT AN ENUMERATION IMPOSING AN "UNNATURAL" ORDER ON ALPHABETIC
--    CHARACTERS CORRECTLY EVALUATES THE ORDERING OPERATORS.

-- RM    15 OCTOBER 1980
-- JWC 7/8/85 RENAMED TO -AB

with Report;
procedure C45210a is

   use Report;

   type T is ('S', 'P', 'M', 'R');

   Mvar : T := T'('M');
   Pvar : T := T'('P');
   Rvar : T := T'('R');
   Svar : T := T'('S');

   Error_Count : Integer := 0;   -- INITIAL VALUE ESSENTIAL

   procedure Bump is
   begin
      Error_Count := Error_Count + 1;
   end Bump;

begin

   Test
     ("C45210A",
      "CHECK THAT AN ENUMERATION IMPOSING" &
      " AN ""UNNATURAL"" ORDER ON ALPHABETIC" &
      " CHARACTERS  CORRECTLY EVALUATES THE " &
      " ORDERING OPERATORS");

   -- 256 CASES ( 4 * 4 ORDERED PAIRS OF OPERAND VALUES,
   --               4    ORDERING OPERATORS: '<' , '<=' , '>' , '>='
   --                         (IN THE TABLE:  A  ,  B   ,  C  ,  D   )
   --               4    VARIABLE/LITERAL FOR LEFT OPERAND,
   --                    VARIABLE/LITERAL FOR RIGHT OPERAND,
   --                         (IN THE TABLE:  VV = ALPHA ,
   --                                         VL = BETA  ,
   --                                         LV = GAMMA ,
   --                                         LL = DELTA  ) RANDOMIZED
   --    INTO 16 (ONE FOR EACH PAIR OF VALUES) ACCORDING TO THE FOL-
   --    LOWING GRAECO-LATIN SQUARE (WITH ADDITIONAL PROPERTIES):

   --               RIGHT OPERAND:    'S'      'P'      'M'      'R'
   --         LEFT
   --       OPERAND:

   --         'S'                   A-ALPHA  B-BETA   C-GAMMA  D-DELTA
   --         'P'                   C-DELTA  D-GAMMA  A-BETA   B-ALPHA
   --         'M'                   D-BETA   C-ALPHA  B-DELTA  A-GAMMA
   --         'R'                   B-GAMMA  A-DELTA  D-ALPHA  C-BETA

   --    (BOTH THE LATIN DIAGONAL AND THE GREEK DIAGONAL CONTAIN 4
   --    DISTINCT LETTERS, NON-TRIVIALLY PERMUTED.)

   -- THE ABOVE DESCRIBES PART 1 OF THE TEST. PART 2 PERFORMS AN
   --    EXHAUSTIVE VERIFICATION OF THE 'VARIABLE VS. VARIABLE' CASE
   --    ( VV , ALPHA ) FOR ALL 4 OPERATORS.

   -----------------------------------------------------------------

   -- PART 1

   --  'BUMP' MEANS 'BUMP THE ERROR COUNT'

   if T'(Svar) < T'(Svar) then
      Bump;
   end if;
   if T'(Svar) <= T'('P') then
      null;
   else
      Bump;
   end if;
   if T'('S') > T'(Mvar) then
      Bump;
   end if;
   if T'('S') >= T'('R') then
      Bump;
   end if;

   if T'('P') > T'('S') then
      null;
   else
      Bump;
   end if;
   if T'('P') >= T'(Pvar) then
      null;
   else
      Bump;
   end if;
   if T'(Pvar) < T'('M') then
      null;
   else
      Bump;
   end if;
   if T'(Pvar) <= T'(Rvar) then
      null;
   else
      Bump;
   end if;

   if T'(Mvar) >= T'('S') then
      null;
   else
      Bump;
   end if;
   if T'(Mvar) > T'(Pvar) then
      null;
   else
      Bump;
   end if;
   if T'('M') <= T'('M') then
      null;
   else
      Bump;
   end if;
   if T'('M') < T'(Rvar) then
      null;
   else
      Bump;
   end if;

   if T'('R') <= T'(Svar) then
      Bump;
   end if;
   if T'('R') < T'('P') then
      Bump;
   end if;
   if T'(Rvar) >= T'(Mvar) then
      null;
   else
      Bump;
   end if;
   if T'(Rvar) > T'('R') then
      Bump;
   end if;

   if Error_Count /= 0 then
      Failed ("""UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE1");
   end if;

   -----------------------------------------------------------------

   -- PART 2

   -- 'BUMP' MEANS 'INCREASE THE COUNT FOR THE NUMBER OF <TRUE>S'

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 4 VALUES
      for Bvar in T'First .. T'('P') loop    -- 2 VALUES

         if Avar < Bvar then
            Bump;
         end if;   -- COUNT +:=  1

      end loop;
   end loop;

   if Error_Count /= 1 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("""UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE2");
   end if;

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 4 VALUES
      for Bvar in T'First .. T'('P') loop    -- 2 VALUES

         if Avar <= Bvar then
            Bump;
         end if;   -- COUNT +:=  3

      end loop;
   end loop;

   if Error_Count /= 3 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("""UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE3");
   end if;

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 4 VALUES
      for Bvar in T'First .. T'('P') loop    -- 2 VALUES

         if Avar > Bvar then
            Bump;
         end if;   -- COUNT +:=  5

      end loop;
   end loop;

   if Error_Count /= 5 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("""UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE4");
   end if;

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 4 VALUES
      for Bvar in T'First .. T'('P') loop    -- 2 VALUES

         if Avar >= Bvar then
            Bump;
         end if;   -- COUNT +:=  7

      end loop;
   end loop;

   if Error_Count /= 7 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("""UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE5");
   end if;

   Result;

end C45210a;

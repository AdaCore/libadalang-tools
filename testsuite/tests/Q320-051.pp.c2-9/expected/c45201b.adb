-- C45201B.ADA

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
-- CHECK THAT THE ORDERING OF ENUMERATION LITERALS AS DEFINED BY THE
--    ORDERING OPERATORS IS THE SAME AS THE ORDER OF OCCURRENCE OF THE
--    LITERALS IN THE TYPE DEFINITION.

-- THIS TEST IS DERIVED FROM C45210A.ADA .

-- RM    17 OCTOBER 1980
-- JWC 7/8/85 RENAMED TO -AB

with Report;
procedure C45201b is

   use Report;

   type T is (A, Slit, B, Plit, C, Nul, D, 'R', E);

   --                 S-LIT ,    P-LIT ,    NUL ,     'R'   CORRESPOND
   --            TO    'S'  ,     'P'  ,    'M'  ,    'R'  IN C45210A.

   subtype T1 is T range A .. B;
   subtype T2 is T range A .. C;    -- INCLUDES  T1
   subtype T3 is T range B .. D;    -- INTERSECTS  T2 , T4
   subtype T4 is T range C .. E;    -- DISJOINT FROM  T1 , T2

   Mvar : T3 := T'(Nul);
   Pvar : T2 := T'(Plit);
   Rvar : T4 := T'('R');
   Svar : T1 := T'(Slit);

   Error_Count : Integer := 0;   -- INITIAL VALUE ESSENTIAL

   procedure Bump is
   begin
      Error_Count := Error_Count + 1;
   end Bump;

begin

   Test
     ("C45201B",
      "CHECK THAT THE ORDERING OF ENUMERATION LITERALS " &
      " AS DEFINED BY THE ORDERING OPERATORS" &
      " IS THE SAME AS THE ORDER OF OCCURRENCE OF THE " &
      " LITERALS IN THE TYPE DEFINITION");

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
   if T'(Svar) <= T'(Plit) then
      null;
   else
      Bump;
   end if;
   if T'(Slit) > T'(Mvar) then
      Bump;
   end if;
   if T'(Slit) >= T'('R') then
      Bump;
   end if;

   if T'(Plit) > T'(Slit) then
      null;
   else
      Bump;
   end if;
   if T'(Plit) >= T'(Pvar) then
      null;
   else
      Bump;
   end if;
   if T'(Pvar) < T'(Nul) then
      null;
   else
      Bump;
   end if;
   if T'(Pvar) <= T'(Rvar) then
      null;
   else
      Bump;
   end if;

   if T'(Mvar) >= T'(Slit) then
      null;
   else
      Bump;
   end if;
   if T'(Mvar) > T'(Pvar) then
      null;
   else
      Bump;
   end if;
   if T'(Nul) <= T'(Nul) then
      null;
   else
      Bump;
   end if;
   if T'(Nul) < T'(Rvar) then
      null;
   else
      Bump;
   end if;

   if T'('R') <= T'(Svar) then
      Bump;
   end if;
   if T'('R') < T'(Plit) then
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
      Failed ("ORDERING OF ENUMERATION LITERALS - FAILURE1");
   end if;

   -----------------------------------------------------------------

   -- PART 2

   -- 'BUMP' MEANS 'INCREASE THE COUNT FOR THE NUMBER OF <TRUE>S'

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 9 VALUES
      for Bvar in T'First .. T'(Plit) loop   -- 4 VALUES

         if Avar < Bvar then
            Bump;
         end if;   -- COUNT +:=  6

      end loop;
   end loop;

   if Error_Count /= 6 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("ORDERING OF ENUMERATION LITERALS - FAILURE2");
   end if;

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 9 VALUES
      for Bvar in T'First .. T'(Plit) loop   -- 4 VALUES

         if Avar <= Bvar then
            Bump;
         end if;   -- COUNT +:= 10

      end loop;
   end loop;

   if Error_Count /= 10 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("ORDERING OF ENUMERATION LITERALS - FAILURE3");
   end if;

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 9 VALUES
      for Bvar in T'First .. T'(Plit) loop   -- 4 VALUES

         if Avar > Bvar then
            Bump;
         end if;   -- COUNT +:= 26

      end loop;
   end loop;

   if Error_Count /= 26 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("ORDERING OF ENUMERATION LITERALS - FAILURE4");
   end if;

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 9 VALUES
      for Bvar in T'First .. T'(Plit) loop   -- 4 VALUES

         if Avar >= Bvar then
            Bump;
         end if;   -- COUNT +:= 30

      end loop;
   end loop;

   if Error_Count /= 30 then   -- THIS IS A PLAIN COUNT, NOT AN
      --    ERROR COUNT
      Failed ("ORDERING OF ENUMERATION LITERALS - FAILURE5");
   end if;

   --  'BUMP' MEANS 'BUMP THE ERROR COUNT' (AGAIN)

   Error_Count := 0;

   for Avar in T'First .. T'Last loop           -- 9 VALUES

      for Bvar in T'First .. T'Last loop    -- 9 VALUES

         if (Avar < Bvar) /= (T'Pos (Avar) < T'Pos (Bvar)) then
            Bump;
         end if;

         if (Avar <= Bvar) /= (T'Pos (Avar) <= T'Pos (Bvar)) then
            Bump;
         end if;

         if (Avar > Bvar) /= (T'Pos (Avar) > T'Pos (Bvar)) then
            Bump;
         end if;

         if (Avar >= Bvar) /= (T'Pos (Avar) >= T'Pos (Bvar)) then
            Bump;
         end if;

      end loop;

   end loop;

   if Error_Count /= 0 then   -- REAL ERROR COUNT AGAIN
      Failed ("ORDERING OF ENUMERATION LITERALS - FAILURE6");
   end if;

   Result;

end C45201b;

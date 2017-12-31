-- C32001B.ADA

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
--     CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR ARRAY TYPES, THE
--     SUBTYPE INDICATION AND THE INITIALIZATION EXPRESSIONS ARE
--     EVALUATED ONCE FOR EACH NAMED OBJECT THAT IS DECLARED AND THE
--     SUBTYPE INDICATION IS EVALUATED FIRST.  ALSO, CHECK THAT THE
--     EVALUATIONS YIELD THE SAME RESULT AS A SEQUENCE OF SINGLE OBJECT
--     DECLARATIONS.

-- HISTORY:
--     RJW 07/16/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  CHANGED
--                   COMMENTS FOR S4 AND CS4 TO READ THAT THE BOUNDS ARE
--                   1 .. 6 AND THE COMPONENT TYPE ARR IS 1 .. 5.

with Report; use Report;

procedure C32001b is

   type Arr is array (Natural range <>) of Integer;

   Bump : array (1 .. 4) of Integer := (0, 0, 0, 0);

   function F (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      return Bump (I);
   end F;

begin
   Test
     ("C32001B",
      "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " &
      "FOR ARRAY TYPES, THE SUBTYPE INDICATION " &
      "AND THE INITIALIZATION EXPRESSIONS ARE " &
      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
      "EVALUATIONS YIELD THE SAME RESULT AS A " &
      "SEQUENCE OF SINGLE OBJECT DECLARATIONS");

   declare

      S1, S2   : Arr (1 .. F (1))          := (others => F (1));
      Cs1, Cs2 : constant Arr (1 .. F (2)) := (others => F (2));

      procedure Check (A, B : Arr; Str1, Str2 : String) is
      begin
         if A'Last /= 1 then
            Failed ("INCORRECT UPPER BOUND FOR " & Str1);
         end if;

         if A (1) /= 2 then
            Failed ("INCORRECT INITIAL VALUE FOR " & Str1);
         end if;

         if B'Last /= 3 then
            Failed ("INCORRECT UPPER BOUND FOR " & Str2);
         end if;

         begin
            if B (1 .. 3) = (4, 5, 6) then
               Comment (Str2 & " WAS INITIALIZED TO " & "(4, 5, 6)");
            elsif B (1 .. 3) = (5, 4, 6) then
               Comment (Str2 & " WAS INITIALIZED TO " & "(5, 4, 6)");
            elsif B (1 .. 3) = (4, 6, 5) then
               Comment (Str2 & " WAS INITIALIZED TO " & "(4, 6, 5)");
            elsif B (1 .. 3) = (6, 4, 5) then
               Comment (Str2 & " WAS INITIALIZED TO " & "(6, 4, 5)");
            elsif B (1 .. 3) = (6, 5, 4) then
               Comment (Str2 & " WAS INITIALIZED TO " & "(6, 5, 4)");
            elsif B (1 .. 3) = (5, 6, 4) then
               Comment (Str2 & " WAS INITIALIZED TO " & "(5, 6, 4)");
            else
               Failed (Str2 & " HAS INCORRECT INITIAL " & "VALUE");
            end if;
         exception
            when Constraint_Error =>
               Failed ("CONSTRAINT_ERROR RAISED - " & Str2);
            when others =>
               Failed ("EXCEPTION RAISED - " & Str2);
         end;
      end Check;

   begin
      Check (S1, S2, "S1", "S2");
      Check (Cs1, Cs2, "CS1", "CS2");
   end;

   declare

      S3, S4 : array (1 .. F (3)) of Arr (1 .. F (3)) :=
        (others => (others => F (3)));

      Cs3, Cs4 : constant array (1 .. F (4)) of Arr (1 .. F (4)) :=
        (others => (others => F (4)));
   begin
      if S3'Last = 1 then
         if S3 (1)'Last = 2 then
            Comment
              ("S3 HAS BOUNDS 1 .. 1 AND " & "COMPONENT TYPE ARR (1 .. 2)");
            if S3 (1) (1 .. 2) = (3, 4) then
               Comment ("S3 HAS INITIAL VALUES " & "3 AND 4 - 1");
            elsif S3 (1) (1 .. 2) = (4, 3) then
               Comment ("S3 HAS INITIAL VALUES " & "4 AND 3 - 1");
            else
               Failed ("S3 HAS WRONG INITIAL VALUES - 1");
            end if;
         else
            Failed ("S3 HAS WRONG COMPONENT TYPE - 1");
         end if;
      elsif S3'Last = 2 then
         if S3 (1)'Last = 1 then
            Comment
              ("S3 HAS BOUNDS 1 .. 2 AND " & "COMPONENT TYPE ARR (1 .. 1)");
            if S3 (1) (1) = 3 and S3 (2) (1) = 4 then
               Comment ("S3 HAS INITIAL VALUES " & "3 AND 4 - 2");
            elsif S3 (1) (1) = 4 and S3 (2) (1) = 3 then
               Comment ("S3 HAS INITIAL VALUES " & "4 AND 3 - 2");
            else
               Failed ("S3 HAS WRONG INITIAL VALUES - 2");
            end if;
         else
            Failed ("S3 HAS WRONG COMPONENT TYPE - 2");
         end if;
      else
         Failed ("S3 HAS INCORRECT BOUNDS");
      end if;

      if S4'Last = 5 then
         if S4 (1)'Last = 6 then
            Comment
              ("S4 HAS BOUNDS 1 .. 5 AND " & "COMPONENT TYPE ARR (1 .. 6)");
         else
            Failed ("S4 HAS WRONG COMPONENT TYPE - 1");
         end if;
      elsif S4'Last = 6 then
         if S4 (1)'First = 1 and S4 (1)'Last = 5 then
            Comment
              ("S4 HAS BOUNDS 1 .. 6 AND " & "COMPONENT TYPE ARR (1 .. 5)");
         else
            Failed ("S4 HAS WRONG COMPONENT TYPE - 2");
         end if;
      else
         Failed ("S4 HAS INCORRECT BOUNDS");
      end if;

      if Bump (3) /= 36 then
         Failed
           ("FUNCTION F NOT INVOKED CORRECT NUMBER OF " &
            "TIMES TO INITIALIZE S4");
      end if;

      if Cs3'First = 1 and Cs3'Last = 1 then
         if Cs3 (1)'First = 1 and Cs3 (1)'Last = 2 then
            Comment
              ("CS3 HAS BOUNDS 1 .. 1 AND " & "COMPONENT TYPE ARR (1 .. 2)");
            if Cs3 (1) (1 .. 2) = (3, 4) then
               Comment ("CS3 HAS INITIAL VALUES " & "3 AND 4 - 1");
            elsif Cs3 (1) (1 .. 2) = (4, 3) then
               Comment ("CS3 HAS INITIAL VALUES " & "4 AND 3 - 1");
            else
               Failed ("CS3 HAS WRONG INITIAL VALUES - 1");
            end if;
         else
            Failed ("CS3 HAS WRONG COMPONENT TYPE - 1");
         end if;
      elsif Cs3'First = 1 and Cs3'Last = 2 then
         if Cs3 (1)'First = 1 and Cs3 (1)'Last = 1 then
            Comment
              ("CS3 HAS BOUNDS 1 .. 2 AND " & "COMPONENT TYPE ARR (1 .. 1)");
            if Cs3 (1) (1) = 3 and Cs3 (2) (1) = 4 then
               Comment ("CS3 HAS INITIAL VALUES " & "3 AND 4 - 2");
            elsif Cs3 (1) (1) = 4 and Cs3 (2) (1) = 3 then
               Comment ("CS3 HAS INITIAL VALUES " & "4 AND 3 - 2");
            else
               Failed ("CS3 HAS WRONG INITIAL VALUES - 2");
            end if;
         else
            Failed ("CS3 HAS WRONG COMPONENT TYPE - 2");
         end if;
      else
         Failed ("CS3 HAS INCORRECT BOUNDS");
      end if;

      if Cs4'First = 1 and Cs4'Last = 5 then
         if Cs4 (1)'First = 1 and Cs4 (1)'Last = 6 then
            Comment
              ("CS4 HAS BOUNDS 1 .. 5 AND " & "COMPONENT TYPE ARR (1 .. 6)");
         else
            Failed ("CS4 HAS WRONG COMPONENT TYPE - 1");
         end if;
      elsif Cs4'First = 1 and Cs4'Last = 6 then
         if Cs4 (1)'First = 1 and Cs4 (1)'Last = 5 then
            Comment
              ("CS4 HAS BOUNDS 1 .. 6 AND " & "COMPONENT TYPE ARR (1 .. 5)");
         else
            Failed ("CS4 HAS WRONG COMPONENT TYPE - 2");
         end if;
      else
         Failed ("CS4 HAS INCORRECT BOUNDS");
      end if;

      if Bump (4) /= 36 then
         Failed
           ("FUNCTION F NOT INVOKED CORRECT NUMBER OF " &
            "TIMES TO INITIALIZE CS4");
      end if;
   end;

   Result;
end C32001b;

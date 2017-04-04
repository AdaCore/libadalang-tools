-- C57004A.ADA

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
-- CHECK THAT AN EXIT STATEMENT WITH A LOOP NAME TERMINATES EXECUTION
--    OF THE LOOP STATEMENT WHOSE NAME IT MENTIONS, AND OF ALL OTHER
--    LOOP STATEMENTS (IF ANY) INTERIOR TO THE FIRST LOOP AND ENCLOSING
--    THE EXIT STATEMENT.

-- CASE 1 :  UNCONDITIONAL EXITS.

-- RM 04/24/81
-- SPS 3/7/83

with Report;
procedure C57004a is

   use Report;

begin

   Test
     ("C57004A",
      "CHECK THAT A NAMING EXIT STATEMENT TERMINATES" &
      " EXECUTION OF THE NAMED LOOP AND OF ALL LOOPS" &
      " SITUATED IN-BETWEEN");

   declare

      Count : Integer := 0;

   begin

      Outermost :
      for X in Integer range 1 .. 2 loop

         for Y in Integer range 1 .. 2 loop

            Comment ("BEFORE 1");

            Loop1 :
            for I in 1 .. 10 loop
               Comment ("INSIDE 1");
               exit Loop1;
               Failed ("EXIT NOT OBEYED (1)");
               for J in 1 .. 10 loop
                  Failed ("OUTER EXIT NOT OBEYED (1)");
                  exit;
                  Failed ("BOTH EXITS IGNORED (1)");
               end loop;
            end loop Loop1;

            Comment ("BEFORE 2");
            Count := Count + 1;

            Loop2 :
            for A in 1 .. 1 loop
               for B in 1 .. 1 loop

                  for I in Character loop
                     Comment ("INSIDE 2");
                     exit Loop2;
                     Failed ("EXIT NOT OBEYED (2)");
                     for J in Boolean loop
                        Failed ("OUTER EXIT NOT " & "OBEYED (2)");
                        exit;
                        Failed ("BOTH EXITS IGNORED " & "(2)");
                     end loop;
                  end loop;

               end loop;
            end loop Loop2;

            Comment ("BEFORE 3");
            Count := Count + 1;

            Loop3 :
            for A in 1 .. 1 loop
               for B in 1 .. 1 loop

                  for I in Boolean loop
                     Comment ("INSIDE 3");
                     begin
                        exit Loop3;
                        Failed ("EXIT NOT OBEYED (3)");
                     end;
                     Failed ("EXIT NOT OBEYED (3BIS)");
                  end loop;

               end loop;
            end loop Loop3;

            Comment ("BEFORE 4");
            Count := Count + 1;

            Loop4 :
            for A in 1 .. 1 loop
               for B in 1 .. 1 loop

                  for I in Integer range 1 .. 10 loop
                     Comment ("INSIDE 4");
                     case A is
                        when 1 =>
                           exit Loop4;
                           Failed ("EXIT NOT OBEYED " & "(4)");
                     end case;
                     Failed ("EXIT NOT OBEYED (4BIS)");
                  end loop;

               end loop;
            end loop Loop4;

            Comment ("AFTER 4");
            Count := Count + 1;
            exit Outermost;

         end loop;

         Failed ("MISSED FINAL EXIT");

      end loop Outermost;

      if Count /= 4 then
         Failed ("WRONG FLOW OF CONTROL");
      end if;

   end;

   Result;

end C57004a;

-- C84002A.ADA

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
-- CHECK THAT:

--   A) IF A USE CLAUSE NAMES AN ENCLOSING PACKAGE, THE USE CLAUSE
--      HAS NO EFFECT.

--   B) IF A DECLARATION IS DIRECTLY VISIBLE PRIOR TO THE OCCURRENCE
--      OF A USE CLAUSE, AND IS NOT IN THE SET OF POTENTIALLY
--      VISIBLE DECLARATIONS, IT REMAINS DIRECTLY VISIBLE AFTER THE
--      USE CLAUSE.

--   C) IF A HOMOGRAPH FOR A POTENTIALLY VISIBLE SUBPROGRAM OR
--      OBJECT IS DECLARED AFTER A USE CLAUSE, THE POTENTIALLY
--      VISIBLE ENTITY IS NO LONGER VISIBLE.

-- EG  02/16/84

with Report;

procedure C84002a is

   use Report;

begin

   Test
     ("C84002A",
      "CHECK THAT DECLARATIONS DIRECTLY VISIBLE PRIOR " &
      "TO THE USE CLAUSE REMAIN VISIBLE AFTERWARDS");

   begin

      Comment
        ("CASE A : CHECK THAT IF A USE CLAUSE NAMES AN " &
         "ENCLOSING PACKAGE, THE USE CLAUSE HAS NO EFFECT");

      Case_A :
      declare

         package P1 is
            X : Float := 1.5;
         end P1;
         package P2 is
            X : Integer := 15;

            use P1;
            use P2;

            A : Integer := X;
         end P2;
         package body P1 is
         begin
            null;
         end P1;
         package body P2 is
         begin
            if X /= Ident_Int (15) or X /= P2.X or A /= P2.X then
               Failed ("CASE A : USE CLAUSE HAS AN EFFECT");
            end if;
         end P2;

      begin

         null;

      end Case_A;

      Comment
        ("CASE B : CHECK THAT IF A DECLARATION IS DIRECTLY " &
         "VISIBLE PRIOR TO THE OCCURRENCE OF A USE CLAUSE, " &
         "AND IS NOT IN THE SET OF POTENTIALLY VISIBLE " &
         "DECLARATIONS, IT REMAINS DIRECTLY VISIBLE");

      Case_B :
      begin

         Case_B1 :
         declare

            package P1 is
               Y : Float := 1.5;
            end P1;
            package P2 is
               X : Integer := 15;

               use P1;

               A : Integer := X;
            end P2;

            package body P1 is
            begin
               null;
            end P1;
            package body P2 is
            begin
               if X /= Ident_Int (15) or X /= P2.X or A /= P2.X then
                  Failed
                    ("CASE B1 : DECLARATION NO " & "LONGER DIRECTLY VISIBLE");
               end if;
            end P2;

         begin

            null;

         end Case_B1;

         Case_B2 :
         declare

            procedure Proc1 (X : String) is
            begin
               null;
            end Proc1;

            package P1 is
               procedure Proc1 (X : String);
            end P1;
            package body P1 is
               procedure Proc1 (X : String) is
               begin
                  Failed ("CASE B2 : WRONG PROCEDURE " & "DIRECTLY VISIBLE");
               end Proc1;
            end P1;

            use P1;

         begin

            Proc1 ("ABC");

         end Case_B2;

         Case_B3 :
         declare

            procedure Proc1 (X : String) is
            begin
               null;
            end Proc1;

            package P1 is
               procedure Proc1 (Y : String);
            end P1;
            package body P1 is
               procedure Proc1 (Y : String) is
               begin
                  Failed ("CASE B3 : WRONG PROCEDURE " & "DIRECTLY VISIBLE");
               end Proc1;
            end P1;

            use P1;

         begin

            Proc1 ("ABC");

         end Case_B3;

      end Case_B;

      Comment
        ("CASE C : IF A HOMOGRAPH FOR A POTENTIALLY " &
         "VISIBLE SUBPROGRAM OR OBJECT IS DECLARED AFTER " &
         "A USE CLAUSE, THE POTENTIALLY VISIBLE ENTITY " &
         "IS NO LONGER VISIBLE");

      Case_C :
      begin

         Case_C1 :
         declare

            package P1 is
               procedure Proc1 (X : Float);
            end P1;

            use P1;

            package body P1 is
               procedure Proc1 (X : Float) is
               begin
                  if X = -1.5 then
                     Failed ("CASE C1 : WRONG PROCEDURE" & " CALLED (A)");
                  elsif X /= 1.5 then
                     Failed ("CASE C1 : WRONG VALUE " & "PASSED (A)");
                  end if;
               end Proc1;
            begin
               null;
            end P1;

            procedure Proc2 is
            begin
               Proc1 (1.5);
            end Proc2;

            procedure Proc1 (X : Float) is
            begin
               if X = 1.5 then
                  Failed ("CASE C1 : WRONG PROCEDURE" & " CALLED (B)");
               elsif X /= -1.5 then
                  Failed ("CASE C1 : WRONG VALUE " & "PASSED (B)");
               end if;
            end Proc1;

         begin

            Proc2;
            Proc1 (-1.5);

         end Case_C1;

         Case_C2 :
         declare

            package P1 is
               X : Integer := 15;
            end P1;

            use P1;

            A : Integer := X;

            X : Boolean := True;

            B : Boolean := X;

         begin

            if A /= Ident_Int (15) then
               Failed
                 ("CASE C2 : VARIABLE A DOES NOT " &
                  "CONTAIN THE CORRECT VALUE");
            end if;
            if B /= Ident_Bool (True) then
               Failed
                 ("CASE C2 : VARIABLE B DOES NOT " &
                  "CONTAIN THE CORRECT VALUE");
            end if;

         end Case_C2;

      end Case_C;

   end;

   Result;

end C84002a;

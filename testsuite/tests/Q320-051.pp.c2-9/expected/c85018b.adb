-- C85018B.ADA

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
--     CHECK THAT WHEN AN ENTRY FAMILY MEMBER IS RENAMED THE FORMAL
--     PARAMETER CONSTRAINTS FOR THE NEW NAME ARE IGNORED IN
--     FAVOR OF THE CONSTRAINTS ASSOCIATED WITH THE RENAMED ENTITY.

-- HISTORY:
--     RJW 06/03/86 CREATED ORIGINAL TEST.
--     DHH 10/15/87 CORRECTED RANGE ERRORS.
--     GJD 11/15/95 REMOVED ADA 95 INCOMPATIBILITY (INDEX CONSTRAINT).
--     PWN 10/24/96 RESTORED CHECKS WITH ADA 95 RESULTS NOW EXPECTED.
--     PWN 12/11/96 ADJUSTED VALUES FOR ADA 95 COMPATIBILITY.
--     PWB.CTA 2/17/97 CHANGED CALL TO ENT2 TO NOT EXPECT EXCEPTION

with Report; use Report;

procedure C85018b is

begin

   Test
     ("C85018B",
      "CHECK THAT WHEN AN ENTRY FAMILY MEMBER IS " &
      "RENAMED THE FORMAL PARAMETER CONSTRAINTS " &
      "FOR THE NEW NAME ARE IGNORED IN FAVOR OF " &
      "THE CONSTRAINTS ASSOCIATED WITH THE RENAMED " &
      "ENTITY");

   declare
      type Int is range 1 .. 10;
      subtype Int1 is Int range 1 .. 5;
      subtype Int2 is Int range 6 .. 10;

      Obj1 : Int1 := 5;
      Obj2 : Int2 := 6;

      subtype Shortchar is Character range 'A' .. 'C';

      task T is
         entry Ent1 (Shortchar) (A : Int1; Ok : Boolean);
      end T;

      procedure Ent2 (A : Int2; Ok : Boolean) renames T.Ent1 ('C');

      task body T is
      begin
         loop
            select
               accept Ent1 ('C') (A : Int1; Ok : Boolean) do
                  if not Ok then
                     Failed ("WRONG CALL EXECUTED " & "WITH INTEGER TYPE");
                  end if;
               end Ent1;
            or
               terminate;
            end select;
         end loop;
      end T;
   begin
      begin
         Ent2 (Obj1, True);
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED WITH " & "INTEGER TYPE");
         when others =>
            Failed ("OTHER EXCEPTION RAISED WITH " & "INTEGER TYPE - 1");
      end;

      begin
         Ent2 (Obj2, True);
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION RAISED WITH " & "INTEGER TYPE - 2");
      end;
   end;

   declare
      type Real is digits 3;
      subtype Real1 is Real range -2.0 .. 0.0;
      subtype Real2 is Real range 0.0 .. 2.0;

      Obj1 : Real1 := -0.25;
      Obj2 : Real2 := 0.25;

      subtype Shortint is Integer range 9 .. 11;

      task T is
         entry Ent1 (Shortint) (A : Real1; Ok : Boolean);
      end T;

      procedure Ent2 (A : Real2; Ok : Boolean) renames T.Ent1 (10);

      task body T is
      begin
         loop
            select
               accept Ent1 (10) (A : Real1; Ok : Boolean) do
                  if not Ok then
                     Failed
                       ("WRONG CALL EXECUTED " &
                        "WITH FLOATING POINT " &
                        "TYPE");
                  end if;
               end Ent1;
            or
               terminate;
            end select;
         end loop;
      end T;
   begin
      begin
         Ent2 (Obj1, True);
      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED WITH " & "FLOATING POINT " & "TYPE");
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED WITH " &
               "FLOATING POINT " &
               "TYPE - 1");
      end;

      begin
         Ent2 (Obj2, False);
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED WITH " &
               "FLOATING POINT " &
               "TYPE - 2");
      end;
   end;

   declare
      type Color is (Red, Yellow, Blue, Green);

      type Fixed is delta 0.125 range -1.0 .. 1.0;
      subtype Fixed1 is Fixed range 0.0 .. 0.5;
      subtype Fixed2 is Fixed range -0.5 .. 0.0;

      Obj1 : Fixed1 := 0.125;
      Obj2 : Fixed2 := -0.125;

      task T is
         entry Ent1 (Color) (A : Fixed1; Ok : Boolean);
      end T;

      procedure Ent2 (A : Fixed2; Ok : Boolean) renames T.Ent1 (Blue);

      task body T is
      begin
         loop
            select
               accept Ent1 (Blue) (A : Fixed1; Ok : Boolean) do
                  if not Ok then
                     Failed
                       ("WRONG CALL EXECUTED " & "WITH FIXED POINT " & "TYPE");
                  end if;
               end Ent1;
            or
               terminate;
            end select;
         end loop;
      end T;
   begin
      begin
         Ent2 (Obj1, True);
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED WITH " & "FIXED POINT " & "TYPE");
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED WITH " & "FIXED POINT " & "TYPE - 1");
      end;

      begin
         Ent2 (Obj2, False);
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED WITH " & "FIXED POINT " & "TYPE - 2");
      end;
   end;

   declare
      type Ta is array (Integer range <>) of Integer;
      subtype Sta1 is Ta (1 .. 5);
      subtype Sta2 is Ta (6 .. 10);

      Obj1 : Sta1 := (1, 2, 3, 4, 5);
      Obj2 : Sta2 := (6, 7, 8, 9, 10);

      task T is
         entry Ent1 (Boolean) (A : Sta1; Ok : Boolean);
      end T;

      procedure Ent2 (A : Sta2; Ok : Boolean) renames T.Ent1 (False);

      task body T is
      begin
         loop
            select
               accept Ent1 (False) (A : Sta1; Ok : Boolean) do
                  if not Ok then
                     Failed
                       ("WRONG CALL EXECUTED " &
                        "WITH CONSTRAINED " &
                        "ARRAY");
                  end if;
               end Ent1;
            or
               terminate;
            end select;
         end loop;
      end T;
   begin
      begin
         Ent2 (Obj1, True);
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED WITH " & "CONSTRAINED ARRAY");
         when others =>
            Failed ("OTHER EXCEPTION RAISED WITH " & "CONSTRAINED ARRAY - 1");
      end;

      begin
         Ent2 (Obj2, True);
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED WITH " & "CONSTRAINED ARRAY");
         when others =>
            Failed ("OTHER EXCEPTION RAISED WITH " & "CONSTRAINED ARRAY - 2");
      end;
   end;

   Result;

end C85018b;

-- C37215F.ADA

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
-- CHECK THAT IF
--        A DISCRIMINANT CONSTRAINT
-- DEPENDS ON A DISCRIMINANT, THE DISCRIMINANT VALUE IS CHECKED FOR
-- COMPATIBILITY WHEN THE RECORD TYPE IS:
--
--   CASE D: USED WITHOUT A CONSTRAINT ONLY IN AN ALLOCATOR OR OBJECT
--      DECLARATION AND THE COMPONENT IS PRESENT IN THE DEFAULT SUBTYPE.

-- JBG 10/17/86
-- PWN 05/31/96 Corrected format of call to "TEST"

with Report; use Report;
procedure C37215f is

   subtype Sm is Integer range 1 .. 10;

   type Rec (D1, D2 : Sm) is record
      null;
   end record;

begin
   Test
     ("C37215F",
      "CHECK EVALUATION OF DISCRIMINANT EXPRESSIONS " &
      "WHEN CONSTRAINT DEPENDS ON DISCRIMINANT, " &
      "DISCRIMINANTS HAVE DEFAULTS, AND COMPONENT " &
      "SUBTYPE DETERMINES WHETHER CONSTRAINT SHOULD " & "BE CHECKED");

-- CASE D1: COMPONENT IS PRESENT

   declare
      type Cons (D3 : Integer := Ident_Int (0)) is record
         case D3 is
            when -5 .. 10 =>
               C1 : Rec (D3, 1);
            when others =>
               C2 : Integer := Ident_Int (0);
         end case;
      end record;
   begin
      begin
         declare
            X : Cons;
         begin
            Failed ("DISCRIMINANT CHECK NOT PERFORMED - 1");
            if X /= (1, (1, 1)) then
               Comment ("SHOULDN'T GET HERE");
            end if;
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 1");
      end;

      begin
         declare
            type Acc_Cons is access Cons;
            X : Acc_Cons;
         begin
            X := new Cons;
            Failed ("DISCRIMINANT CHECK NOT PERFORMED - 2");
            if X.all /= (1, (1, 1)) then
               Comment ("IRRELEVANT");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 2A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 2B");
      end;

      begin
         declare
            subtype Scons is Cons;
         begin
            declare
               X : Scons;
            begin
               Failed ("DISCRIMINANT CHECK NOT " & "PERFORMED - 3");
               if X /= (1, (1, 1)) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 3");
         end;
      exception
         when others =>
            Failed ("CONSTRAINT CHECKED TOO SOON - 3");
      end;

      begin
         declare
            type Arr is array (1 .. 5) of Cons;
         begin
            declare
               X : Arr;
            begin
               Failed ("DISCRIMINANT CHECK NOT " & "PERFORMED - 4");
               if X /= (1 .. 5 => (1, (1, 1))) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 4");
         end;
      exception
         when others =>
            Failed ("CONSTRAINT CHECKED TOO SOON - 4");
      end;

      begin
         declare
            type Nrec is record
               C1 : Cons;
            end record;
         begin
            declare
               X : Nrec;
            begin
               Failed ("DISCRIMINANT CHECK NOT " & "PERFORMED - 5");
               if X /= (C1 => (1, (1, 1))) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 5");
         end;
      exception
         when others =>
            Failed ("CONSTRAINT CHECKED TOO SOON - 5");
      end;

      begin
         declare
            type Drec is new Cons;
         begin
            declare
               X : Drec;
            begin
               Failed ("DISCRIMINANT CHECK NOT " & "PERFORMED - 6");
               if X /= (1, (1, 1)) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 6");
         end;
      exception
         when others =>
            Failed ("CONSTRAINT CHECKED TOO SOON - 6");
      end;

   end;

-- CASE C2 : COMPONENT IS ABSENT

   declare
      type Cons (D3 : Integer := Ident_Int (11)) is record
         case D3 is
            when -5 .. 10 =>
               C1 : Rec (D3, Ident_Int (1));
            when others =>
               C2 : Integer := Ident_Int (5);
         end case;
      end record;
   begin
      begin
         declare
            X : Cons;
         begin
            if X /= (11, 5) then
               Failed ("WRONG VALUE FOR X - 11");
            end if;
         end;
      exception
         when others =>
            Failed ("NONEXISTENT CONSTRAINT CHECKED - 11");
      end;

      begin
         declare
            subtype Scons is Cons;
         begin
            declare
               X : Scons;
            begin
               if X /= (11, 5) then
                  Failed ("X VALUE WRONG - 12");
               end if;
            end;
         end;
      exception
         when others =>
            Failed ("NONEXISTENT CONSTRAINT CHECKED - 12");
      end;

      begin
         declare
            type Arr is array (1 .. 5) of Cons;
            X : Arr;
         begin
            if X /= (1 .. 5 => (11, 5)) then
               Failed ("X VALUE INCORRECT - 13");
            end if;
         end;
      exception
         when others =>
            Failed ("NONEXISTENT CONSTRAINT CHECKED - 13");
      end;

      begin
         declare
            type Nrec is record
               C1 : Cons;
            end record;
            X : Nrec;
         begin
            if X /= (C1 => (11, 5)) then
               Failed ("X VALUE IS INCORRECT - 14");
            end if;
         end;
      exception
         when others =>
            Failed ("NONEXISTENT CONSTRAINT CHECKED - 14");
      end;

      begin
         declare
            type Nrec is new Cons;
            X : Nrec;
         begin
            if X /= (11, 5) then
               Failed ("X VALUE INCORRECT - 15");
            end if;
         end;
      exception
         when others =>
            Failed ("NONEXISTENT CONSTRAINT CHECKED - 15");
      end;

      begin
         declare
            type Acc_Cons is access Cons;
            X : Acc_Cons := new Cons;
         begin
            if X.all /= (11, 5) then
               Failed ("X VALUE INCORRECT - 17");
            end if;
         end;
      exception
         when others =>
            Failed ("NONEXISTENT CONSTRAINT CHECKED - 17");
      end;
   end;

   Result;

exception
   when others =>
      Failed ("CONSTRAINT CHECK DONE TOO EARLY");
      Result;

end C37215f;

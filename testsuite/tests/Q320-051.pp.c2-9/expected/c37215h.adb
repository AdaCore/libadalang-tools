-- C37215H.ADA

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
--      CHECK THAT IF AN INDEX CONSTRAINT DEPENDS ON A DISCRIMINANT,
--      THE DISCRIMINANT VALUE IS CHECKED FOR COMPATIBILITY WHEN THE
--      RECORD TYPE IS:
--
--           CASE D: CONSTRAINED BY DEFAULT AND THE COMPONENT IS
--                   PRESENT IN THE SUBTYPE.

-- HISTORY:
--      JBG 10/17/86  CREATED ORIGINAL TEST.
--      RJW 10/13/87  CORRECTED VARIOUS CONSTRAINT ERRORS IN 'CASE D1'.
--      VCL 03/30/88  CORRECTED VARIOUS CONSTRAINT ERRORS WITH TYPE
--                    DECLARATIONS THROUGHOUT THE TEST.  ADDED SEQUENCE
--                    NUMBERS.

with Report; use Report;
procedure C37215h is

   subtype Sm is Integer range 1 .. 10;
   type My_Arr is array (Sm range <>) of Integer;

   Sequence_Number : Integer;
begin
   Test
     ("C37215H",
      "THE DISCRIMINANT VALUES OF AN INDEX " &
      "CONSTRAINT ARE PROPERLY CHECK FOR " &
      "COMPATIBILITY WHEN THE DISCRIMINANT IS " &
      "DEFINED BY DEFAULT AND THE COMPONENT IS AND " &
      "IS NOT PRESENT IN THE SUBTYPE");

-- CASE D1: COMPONENT IS PRESENT

   Sequence_Number := 1;
   declare
      type Cons (D3 : Integer := Ident_Int (0)) is record
         case D3 is
            when -5 .. 10 =>
               C1 : My_Arr (D3 .. 1);
            when others =>
               C2 : Integer := Ident_Int (0);
         end case;
      end record;
   begin
      begin
         declare
            X : Cons;
         begin
            Failed ("INDEX CHECK NOT PERFORMED - 1");
            if X /= (1, (1, 1)) then
               Comment ("SHOULDN'T GET HERE");
            end if;
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 1");
      end;

      begin
         declare
            subtype Scons is Cons;
         begin
            declare
               X : Scons;
            begin
               Failed ("INDEX CHECK NOT PERFORMED - 2");
               if X /= (1, (1, 1)) then
                  Comment ("IRRELEVANT");
               end if;
            end;
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
            type Arr is array (1 .. 5) of Cons;
         begin
            declare
               X : Arr;
            begin
               Failed ("INDEX CHECK NOT PERFORMED - 3");
               if X /= (1 .. 5 => (1, (1, 1))) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 3A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 3B");
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
               Failed ("INDEX CHECK NOT PERFORMED - 4");
               if X /= (C1 => (1, (1, 1))) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 4A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 4B");
      end;

      begin
         declare
            type Nrec is new Cons;
         begin
            declare
               X : Nrec;
            begin
               Failed ("INDEX CHECK NOT PERFORMED - 5");
               if X /= (1, (1, 1)) then
                  Comment ("IRRELEVANT");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 5A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 5B");
      end;

      begin
         declare
            type Acc_Cons is access Cons;
         begin
            declare
               X : Acc_Cons;
            begin
               X := new Cons;
               Failed ("INDEX CHECK NOT PERFORMED - 6");
               if X.all /= (1, (1, 1)) then
                  Comment ("WRONG VALUE FOR X - 6");
               end if;
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED " & "- 6A");
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 6B");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 6C");
      end;
   end;

-- CASE D2: COMPONENT IS ABSENT

   Sequence_Number := 2;
   declare
      type Cons (D3 : Integer := Ident_Int (11)) is record
         case D3 is
            when -5 .. 10 =>
               C1 : My_Arr (Ident_Int (2) .. D3);
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
               Comment ("X VALUE IS INCORRECT - 11");
            end if;
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 11");
      end;

      begin
         declare
            subtype Scons is Cons;
         begin
            declare
               X : Scons;
            begin
               if X /= (11, 5) then
                  Failed ("X VALUE INCORRECT - 12");
               end if;
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 12A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 12B");
      end;

      begin
         declare
            type Arr is array (1 .. 5) of Cons;
         begin
            declare
               X : Arr;
            begin
               if X /= (1 .. 5 => (11, 5)) then
                  Failed ("X VALUE INCORRECT - 13");
               end if;
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 13A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 13B");
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
               if X /= (C1 => (11, 5)) then
                  Failed ("X VALUE INCORRECT - 14");
               end if;
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 14A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 14B");
      end;

      begin
         declare
            type Nrec is new Cons;
         begin
            declare
               X : Nrec;
            begin
               if X /= (11, 5) then
                  Failed ("X VALUE INCORRECT - 15");
               end if;
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 15A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 15B");
      end;

      begin
         declare
            type Acc_Cons is access Cons;
            X : Acc_Cons;
         begin
            X := new Cons;
            if X.all /= (11, 5) then
               Failed ("X VALUE INCORRECT - 17");
            end if;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 17A");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 17B");
      end;
   end;

   Result;
exception
   when others =>
      Failed
        ("INDEX VALUES CHECKED TOO SOON - " & Integer'Image (Sequence_Number));
      Result;
end C37215h;

-- C37213H.ADA

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
--     CHECK, WHERE AN INDEX CONSTRAINT DEPENDS ON A RECORD
--     DISCRIMINANT WITH A DEFAULT VALUE AND THE RECORD TYPE IS NOT
--     EXPLICITLY CONSTRAINED, THAT THE NON-DISCRIMINANT EXPRESSIONS
--     IN THE INDEX CONSTRAINT ARE:
--          1) EVALUATED WHEN THE RECORD COMPONENT SUBTYPE DEFINITION
--             IS ELABORATED,
--          2) PROPERLY CHECKED FOR COMPATIBILITY ONLY IN AN ALLOCATION
--             OR OBJECT DECLARATION AND ONLY IF THE DISCRIMINANT-
--             DEPENDENT COMPONENT IS PRESENT IN THE SUBTYPE.

-- HISTORY:
--     JBG  10/17/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER; MODIFIED THE CHECK OF
--                    SUBTYPE 'SCONS', IN BOTH SUBPARTS OF THE TEST,
--                    TO INDICATE FAILURE IF CONSTRAINT_ERROR IS RAISED
--                    FOR THE SUBTYPE DECLARATION AND FAILURE IF
--                    CONSTRAINT_ERROR IS NOT RAISED FOR AN OBJECT
--                    DECLARATION OF THIS SUBTYPE; RELOCATED THE CALL TO
--                    REPORT.TEST SO THAT IT COMES BEFORE ANY
--                    DECLARATIONS;  ADDED 'SEQUENCE_NUMBER' TO IDENTIFY
--                    THE CURRENT SUBTEST (FOR EXCEPTIONS); CHANGE THE
--                    TYPE OF THE DISCRIMINANT IN THE RECORD 'CONS'
--                    TO AN INTEGER SUBTYPE.
--     VCL  03/30/88  MODIFIED HEADER AND MESSAGES OUTPUT BY REPORT
--                    PACKAGE.

with Report; use Report;
procedure C37213h is
begin
   Test
     ("C37213H",
      "THE NON-DISCRIMINANT EXPRESSIONS OF AN " &
      "INDEX CONSTRAINT THAT DEPEND ON A " &
      "DISCRIMINANT WITH A DEFAULT VALUE ARE " &
      "PROPERLY EVALUATED AND CHECKED WHEN THE " &
      "RECORD TYPE IS NOT EXPLICITLY CONSTRAINED AND " &
      "THE COMPONENT IS AND IS NOT PRESENT IN THE " &
      "SUBTYPE");

   declare
      Sequence_Number : Integer;

      subtype Discr is Integer range -50 .. 50;
      subtype Sm is Integer range 1 .. 10;
      type My_Arr is array (Sm range <>) of Integer;

      F1_Cons : Integer := 2;

      function Chk
        (Cons    : Integer;
         Value   : Integer;
         Message : String) return Boolean
      is
      begin
         if Cons /= Value then
            Failed (Message & ": F1_CONS IS " & Integer'Image (F1_Cons));
         end if;
         return True;
      end Chk;

      function F1 return Integer is
      begin
         F1_Cons := F1_Cons - Ident_Int (1);
         return F1_Cons;
      end F1;
   begin

-- CASE 1: DISCRIMINANT-DEPENDENT COMPONENT IS PRESENT.

      Sequence_Number := 1;
      declare
         type Cons (D3 : Discr := Ident_Int (1)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (F1 .. D3); -- F1 EVALUATED.
               when others =>
                  C2 : Integer := Ident_Int (0);
            end case;
         end record;

         Chk1 : Boolean := Chk (F1_Cons, 1, "F1 NOT EVALUATED");

         X : Cons;                     -- F1 NOT EVALUATED AGAIN.
         Y : Cons;                     -- F1 NOT EVALUATED AGAIN.

         Chk2 : Boolean := Chk (F1_Cons, 1, "F1 EVALUATED");
      begin
         if X.C1'First /= 1 or Y.C1'Last /= 1 then
            Failed ("VALUES NOT CORRECT");
         end if;
      end;

      F1_Cons := 12;

      Sequence_Number := 2;
      declare
         type Cons (D3 : Discr := Ident_Int (1)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (D3 .. F1);
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
                  Comment ("INCORRECT VALUES FOR X - 1");
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
                     Comment ("INCORRECT VALUES FOR X " & "- 2");
                  end if;
               end;
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED " & "- 2A");
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
                     Comment ("INCORRECT VALUES FOR X " & "- 3");
                  end if;
               end;
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED " & "- 3A");
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
                     Comment ("INCORRECT VALUES FOR X " & "- 4");
                  end if;
               end;
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED " & "- 4A");
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
                     Comment ("INCORRECT VALUES FOR X " & "- 5");
                  end if;
               end;
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED " & "- 5A");
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
                     Comment ("INCORRECT VALUES FOR X " & "- 6");
                  end if;
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Comment ("UNEXPECTED EXCEPTION " & "RAISED - 6A");
               end;
            exception
               when others =>
                  Comment ("UNEXPECTED EXCEPTION RAISED " & "- 6B");
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 6C");
         end;
      end;

-- CASE D2: DISCRIMINANT-DEPENDENT COMPONENT IS ABSENT.

      F1_Cons := 2;

      Sequence_Number := 3;
      declare
         type Cons (D3 : Discr := Ident_Int (-6)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (D3 .. F1); -- F1 EVALUATED.
               when others =>
                  C2 : Integer := Ident_Int (0);
            end case;
         end record;
         Chk1 : Boolean := Chk (F1_Cons, 1, "F1 NOT EVALUATED");

         X : Cons;                      -- F1 NOT EVALUATED AGAIN.
         Y : Cons;                      -- F1 NOT EVALUATED AGAIN.

         Chk2 : Boolean := Chk (F1_Cons, 1, "F1 EVALUATED");
      begin
         if X /= (-6, 0) or Y /= (-6, 0) then
            Failed ("VALUES NOT CORRECT");
         end if;
      end;

      F1_Cons := 12;

      Sequence_Number := 4;
      declare
         type Cons (D3 : Discr := Ident_Int (11)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (D3 .. F1);
               when others =>
                  C2 : Integer := Ident_Int (0);
            end case;
         end record;
      begin
         begin
            declare
               X : Cons;
            begin
               if X /= (11, 0) then
                  Failed ("X VALUE IS INCORRECT - 11");
               end if;
            end;
         exception
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
                  if X /= (11, 0) then
                     Failed ("X VALUE INCORRECT - 12");
                  end if;
               end;
            exception
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED - " & "12A");
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
                  if X /= (1 .. 5 => (11, 0)) then
                     Failed ("X VALUE INCORRECT - 13");
                  end if;
               end;
            exception
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED - " & "13A");
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
                  if X /= (C1 => (11, 0)) then
                     Failed ("X VALUE INCORRECT - 14");
                  end if;
               end;
            exception
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED - " & "14A");
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
                  if X /= (11, 0) then
                     Failed ("X VALUE INCORRECT - 15");
                  end if;
               end;
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED - " & "15A");
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
               if X.all /= (11, 0) then
                  Failed ("X VALUE INCORRECT - 17");
               end if;
            exception
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED - " & "17A");
            end;
         exception
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 17B");
         end;
      end;

   exception
      when Constraint_Error =>
         Failed
           ("INDEX VALUES IMPROPERLY CHECKED - " &
            Integer'Image (Sequence_Number));
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED " & Integer'Image (Sequence_Number));
   end;

   Result;
end C37213h;

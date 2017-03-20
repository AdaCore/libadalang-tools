-- C83051A.ADA

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
--     CHECK THAT DECLARATIONS IN THE VISIBLE PART OF A PACKAGE NESTED
--     WITHIN THE VISIBLE PART OF A PACKAGE ARE VISIBLE BY SELECTION
--     FROM OUTSIDE THE OUTERMOST PACKAGE.

-- HISTORY:
--     GMT 09/07/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C83051a is

begin
   Test
     ("C83051A",
      "CHECK THAT DECLARATIONS IN THE VISIBLE " &
      "PART OF A PACKAGE NESTED WITHIN THE VISIBLE " &
      "PART OF A PACKAGE ARE VISIBLE BY SELECTION " &
      "FROM OUTSIDE THE OUTERMOST PACKAGE");
   A_Block : declare
      package Apack is
         package Bpack is
            type T1 is (Red, Green);
            type T2a is ('A', 'B', 'C', 'D');
            type T3 is new Boolean;
            type T4 is new Integer range -3 .. 8;
            type T5 is digits 5;
            type T67 is delta 0.5 range -2.0 .. 10.0;
            type T9a is array (Integer range <>) of T3;
            subtype T9b is T9a (1 .. 10);
            type T9c is access T9b;
            type T10 is private;
            V1    : T3           := False;
            Zero  : constant T4  := 0;
            A_Flt : T5           := 3.0;
            A_Fix : T67          := -1.0;
            Ary   : T9a (1 .. 4) := (True, True, True, False);
            P1 : T9c := new T9b'(1 .. 5 => T3'(True), 6 .. 10 => T3'(False));
            C1    : constant T10;

            function Ret_T1 (X : T1) return T1;

            function Ret_Char (X : Character) return T10;

            generic
            procedure Do_Nothing (X : in out T3);
         private
            type T10 is new Character;
            C1 : constant T10 := 'J';
         end Bpack;
      end Apack;

      package body Apack is
         package body Bpack is
            function Ret_T1 (X : T1) return T1 is
            begin
               if X = Red then
                  return Green;
               else
                  return Red;
               end if;
            end Ret_T1;

            function Ret_Char (X : Character) return T10 is
            begin
               return T10 (X);
            end Ret_Char;

            procedure Do_Nothing (X : in out T3) is
            begin
               if X = True then
                  X := False;
               else
                  X := True;
               end if;
            end Do_Nothing;
         end Bpack;
      end Apack;

      procedure New_Do_Nothing is new Apack.Bpack.Do_Nothing;

   begin

      -- A1: VISIBILITY FOR UNOVERLOADED ENUMERATION LITERALS

      if Apack.Bpack.">" (Apack.Bpack.Red, Apack.Bpack.Green) then
         Failed
           ("VISIBILITY FOR UNOVERLOADED ENUMERATION " & "LITERAL BAD - A1");
      end if;

      -- A2: VISIBILITY FOR OVERLOADED
      --     ENUMERATION CHARACTER LITERALS

      if Apack.Bpack."<"
          (Apack.Bpack.T2a'(Apack.Bpack.'C'),
           Apack.Bpack.T2a'(Apack.Bpack.'B'))
      then
         Failed
           ("VISIBILITY FOR OVERLOADED ENUMERATION " & "LITERAL BAD - A2");
      end if;

      -- A3: VISIBILITY FOR A DERIVED BOOLEAN TYPE

      if Apack.Bpack."<"
          (Apack.Bpack.T3'(Apack.Bpack.True),
           Apack.Bpack.False)
      then
         Failed ("VISIBILITY FOR DERIVED BOOLEAN BAD - A3");
      end if;

      -- A4: VISIBILITY FOR AN INTEGER TYPE

      if Apack.Bpack."/=" (Apack.Bpack."MOD" (6, 2), Apack.Bpack.Zero) then
         Failed ("VISIBILITY FOR INTEGER TYPE BAD - A4");
      end if;

      -- A5: VISIBILITY FOR A FLOATING POINT TYPE

      if Apack.Bpack.">" (Apack.Bpack.T5'(2.7), Apack.Bpack.A_Flt) then
         Failed ("VISIBILITY FOR FLOATING POINT BAD - A5");
      end if;

      -- A6: VISIBILITY FOR A FIXED POINT INVOLVING UNARY MINUS

      if Apack.Bpack."<"
          (Apack.Bpack.A_Fix,
           Apack.Bpack.T67'(Apack.Bpack."-" (1.5)))
      then
         Failed ("VISIBILITY FOR FIXED POINT WITH UNARY MINUS " & "BAD - A6");
      end if;

      -- A7: VISIBILITY FOR A FIXED POINT DIVIDED BY INTEGER

      if Apack.Bpack."/="
          (Apack.Bpack.T67 (-0.5),
           Apack.Bpack."/" (Apack.Bpack.A_Fix, 2))
      then
         Failed
           ("VISIBILITY FOR FIXED POINT DIVIDED BY " & "INTEGER BAD - A7");
      end if;

      -- A8: VISIBILITY FOR ARRAY EQUALITY

      if Apack.Bpack."/="
          (Apack.Bpack.Ary,
           (Apack.Bpack.T3 (True),
            Apack.Bpack.T3 (True),
            Apack.Bpack.T3 (True),
            Apack.Bpack.T3 (False)))
      then
         Failed ("VISIBILITY FOR ARRAY EQUALITY BAD - A8");
      end if;

      -- A9: VISIBILITY FOR ACCESS EQUALITY

      if Apack.Bpack."/="
          (Apack.Bpack.P1 (3),
           Apack.Bpack.T3 (Ident_Bool (True)))
      then
         Failed ("VISIBILITY FOR ACCESS EQUALITY BAD - A9");
      end if;

      -- A10: VISIBILITY FOR PRIVATE TYPE

      if Apack.Bpack."/=" (Apack.Bpack.C1, Apack.Bpack.Ret_Char ('J')) then
         Failed ("VISIBILITY FOR PRIVATE TYPE BAD - A10");
      end if;

      -- A11: VISIBILITY FOR DERIVED SUBPROGRAM

      if Apack.Bpack."/="
          (Apack.Bpack.Ret_T1 (Apack.Bpack.Red),
           Apack.Bpack.Green)
      then
         Failed ("VISIBILITY FOR DERIVED SUBPROGRAM BAD - A11");
      end if;

      -- A12: VISIBILITY FOR GENERIC SUBPROGRAM

      New_Do_Nothing (Apack.Bpack.V1);

      if Apack.Bpack."/=" (Apack.Bpack.V1, Apack.Bpack.T3 (True)) then
         Failed ("VISIBILITY FOR GENERIC SUBPROGRAM BAD - A12");
      end if;

   end A_Block;

   B_Block : declare
      generic
         type T1 is (<>);
      package Genpack is
         package Apack is
            package Bpack is
               type T1 is (Orange, Green);
               type T2a is ('E', 'F', 'G');
               type T3 is new Boolean;
               type T4 is new Integer range -3 .. 8;
               type T5 is digits 5;
               type T67 is delta 0.5 range -3.0 .. 25.0;
               type T9a is array (Integer range <>) of T3;
               subtype T9b is T9a (2 .. 8);
               type T9c is access T9b;
               type T10 is private;
               V1    : T3           := True;
               Six   : T4           := 6;
               B_Flt : T5           := 4.0;
               Ary   : T9a (1 .. 4) := (True, False, True, False);
               P1 : T9c := new T9b'(2 .. 4 => T3'(False), 5 .. 8 => T3'(True));
               K1    : constant T10;

               function Ret_T1 (X : T1) return T1;

               function Ret_Char (X : Character) return T10;

               generic
               procedure Do_Nothing (X : in out T3);
            private
               type T10 is new Character;
               K1 : constant T10 := 'V';
            end Bpack;
         end Apack;
      end Genpack;

      package body Genpack is
         package body Apack is
            package body Bpack is
               function Ret_T1 (X : T1) return T1 is
               begin
                  if X = Orange then
                     return Green;
                  else
                     return Orange;
                  end if;
               end Ret_T1;

               function Ret_Char (X : Character) return T10 is
               begin
                  return T10 (X);
               end Ret_Char;

               procedure Do_Nothing (X : in out T3) is
               begin
                  if X = True then
                     X := False;
                  else
                     X := True;
                  end if;
               end Do_Nothing;
            end Bpack;
         end Apack;
      end Genpack;

      package Mypack is new Genpack (T1 => Integer);

      procedure My_Do_Nothing is new Mypack.Apack.Bpack.Do_Nothing;

   begin

      -- B1: GENERIC INSTANCE OF UNOVERLOADED ENUMERATION LITERAL

      if Mypack.Apack.Bpack."<"
          (Mypack.Apack.Bpack.Green,
           Mypack.Apack.Bpack.Orange)
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF " &
            "UNOVERLOADED ENUMERATION LITERAL BAD - B1");
      end if;

      -- B2: GENERIC INSTANCE OF OVERLOADED ENUMERATION LITERAL

      if Mypack.Apack.Bpack.">"
          (Mypack.Apack.Bpack.T2a'(Mypack.Apack.Bpack.'F'),
           Mypack.Apack.Bpack.T2a'(Mypack.Apack.Bpack.'G'))
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF " &
            "OVERLOADED ENUMERATION LITERAL BAD - B2");
      end if;

      -- B3: VISIBILITY FOR GENERIC INSTANCE OF DERIVED BOOLEAN

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack."NOT"
             (Mypack.Apack.Bpack.T3'(Mypack.Apack.Bpack.True)),
           Mypack.Apack.Bpack.False)
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF DERIVED " &
            "BOOLEAN BAD - B3");
      end if;

      -- B4: VISIBILITY FOR GENERIC INSTANCE OF DERIVED INTEGER

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack."MOD" (Mypack.Apack.Bpack.Six, 2),
           0)
      then
         Failed ("VISIBILITY FOR GENERIC INSTANCE OF INTEGER " & "BAD - B4");
      end if;

      -- B5: VISIBILITY FOR GENERIC INSTANCE OF FLOATING POINT

      if Mypack.Apack.Bpack.">"
          (Mypack.Apack.Bpack.T5'(1.9),
           Mypack.Apack.Bpack.B_Flt)
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF FLOATING " & "POINT BAD - B5");
      end if;

      -- B6: VISIBILITY FOR GENERIC INSTANCE OF
      --     FIXED POINT UNARY PLUS

      if Mypack.Apack.Bpack."<"
          (2.5,
           Mypack.Apack.Bpack.T67'(Mypack.Apack.Bpack."+" (1.75)))
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF FIXED " &
            "POINT UNARY PLUS BAD - B6");
      end if;

      -- B7: VISIBILITY FOR GENERIC INSTANCE OF
      --     FIXED POINT DIVIDED BY INTEGER

      if Mypack.Apack.Bpack."/=" (Mypack.Apack.Bpack."/" (2.5, 4), 0.625) then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF FIXED " &
            "POINT DIVIDED BY INTEGER BAD - B7");
      end if;

      -- B8: VISIBILITY FOR GENERIC INSTANCE OF ARRAY EQUALITY

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack.Ary,
           (Mypack.Apack.Bpack.T3 (True),
            Mypack.Apack.Bpack.T3 (False),
            Mypack.Apack.Bpack.T3 (True),
            Mypack.Apack.Bpack.T3 (False)))
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF ARRAY " & "EQUALITY BAD - B8");
      end if;

      -- B9: VISIBILITY FOR GENERIC INSTANCE OF ACCESS EQUALITY

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack.P1 (3),
           Mypack.Apack.Bpack.T3 (Ident_Bool (False)))
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF ACCESS " &
            "EQUALITY BAD - B9");
      end if;

      -- B10: VISIBILITY FOR GENERIC INSTANCE OF PRIVATE EQUALITY

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack.K1,
           Mypack.Apack.Bpack.Ret_Char ('V'))
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF PRIVATE " &
            "EQUALITY BAD - B10");
      end if;

      -- B11: VISIBILITY FOR GENERIC INSTANCE OF DERIVED SUBPROGRAM

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack.Ret_T1 (Mypack.Apack.Bpack.Orange),
           Mypack.Apack.Bpack.Green)
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF DERIVED " &
            "SUBPROGRAM BAD - B11");
      end if;

      -- B12: VISIBILITY FOR GENERIC INSTANCE OF GENERIC SUBPROGRAM

      My_Do_Nothing (Mypack.Apack.Bpack.V1);

      if Mypack.Apack.Bpack."/="
          (Mypack.Apack.Bpack.V1,
           Mypack.Apack.Bpack.T3 (False))
      then
         Failed
           ("VISIBILITY FOR GENERIC INSTANCE OF GENERIC " &
            "SUBPROGRAM BAD - B12");
      end if;

   end B_Block;

   Result;
end C83051a;

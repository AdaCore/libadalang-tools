-- C41103B.ADA

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
--     CHECK THAT THE NAME IN AN INDEXED_COMPONENT MAY BE:
--        AN IDENTIFIER DENOTING AN ARRAY OBJECT - N1;
--        AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE
--           DESIGNATES AN ARRAY OBJECT - N2;
--        A FUNCTION CALL DELIVERING AN ARRAY OBJECT USING
--           PREDEFINED FUNCTIONS - &, AND THE LOGICAL OPERATORS
--        A USER-DEFINED FUNCTION - F1;
--        A FUNCTION CALL DELIVERING AN ACCESS VALUE THAT
--           DESIGNATES AN ARRAY - F2;
--        A SLICE (CHECKING UPPER AND LOWER BOUND COMPONENTS) - N3;
--        AN INDEXED COMPONENT DENOTING AN ARRAY OBJECT
--           (ARRAY OF ARRAYS) - N4;
--        AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--           ENCLOSING ITS DECLARATION - C41103B.N1;
--        A RECORD COMPONENT (OF A RECORD CONTAINING ONE OR MORE
--           ARRAYS WHOSE BOUNDS DEPEND ON A DISCRIMINANT) - N5.
--     CHECK THAT THE APPROPRIATE COMPONENT IS ACCESSED (FOR
--     DYNAMIC INDICES).

-- HISTORY:
--     WKB 08/05/81  CREATED ORIGINAL TEST.
--     SPS 10/26/82
--     BCB 08/02/88  MODIFIED HEADER FORMAT AND ADDED CALLS TO THE
--                   LOGICAL OPERATORS.
--     BCB 04/16/90  MODIFIED SLICE TEST TO INCLUDE A READING OF THE
--                   COMPONENT DESIGNATED BY THE LOWER BOUND OF THE
--                   SLICE.  ADDED TEST FOR PREFIX OF INDEXED COMPONENT
--                   HAVING A LIMITED TYPE.
--     PWN 11/30/94  SUBTYPE QUALIFIED LITERALS FOR ADA 9X.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with Report; use Report;
procedure C41103b is

   type A1 is array (Integer range 1 .. 4) of Integer;
   N1 : A1 := (1, 2, 3, 4);

begin
   Test
     ("C41103B",
      "CHECK THAT AN INDEXED_COMPONENT MAY BE OF " &
      "CERTAIN FORMS AND THAT THE APPROPRIATE " &
      "COMPONENT IS ACCESSED (FOR DYNAMIC INDICES)");

   declare

      type A2 is array (Integer range 1 .. 4) of Boolean;
      type A3 is access A1;
      type A4 is array (Integer range 1 .. 4) of A1;
      type R (Length : Integer) is record
         S : String (1 .. Length);
      end record;

      N2 : A3                        := new A1'(1, 2, 3, 4);
      N3 : array (1 .. 7) of Integer := (1, 2, 3, 4, 5, 6, 7);
      N4 : A4                        :=
        (1 => (1, 2, 3, 4),
         2 => (5, 6, 7, 8),
         3 => (9, 10, 11, 12),
         4 => (13, 14, 15, 16));
      N5 : R (4) := (Length => 4, S => "ABCD");

      M2a : A2 := (True, False, True, False);
      M2b : A2 := (True, True, False, False);

      function F1 return A2 is
      begin
         return (False, False, True, False);
      end F1;

      function F2 return A3 is
      begin
         return N2;
      end F2;

      procedure P1
        (X : in     Integer;
         Y : in out Integer;
         Z :    out Integer;
         W : in     String)
      is
      begin
         if X /= 2 then
            Failed ("WRONG VALUE FOR IN PARAMETER - " & W);
         end if;
         if Y /= 3 then
            Failed ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
         end if;
         Y := 8;
         Z := 9;
      end P1;

      procedure P2 (X : Character) is
      begin
         if X /= 'C' then
            Failed ("WRONG VALUE FOR IN PARAMETER - '&'");
         end if;
      end P2;

      procedure P3 (X : Boolean) is
      begin
         if X /= True then
            Failed ("WRONG VALUE FOR IN PARAMETER - F1");
         end if;
      end P3;

      procedure P5
        (X : in     Character;
         Y : in out Character;
         Z :    out Character)
      is
      begin
         if X /= 'A' then
            Failed ("WRONG VALUE FOR IN PARAMETER - N5");
         end if;
         if Y /= 'D' then
            Failed ("WRONG VALUE FOR IN OUT PARAMETER - N5");
         end if;
         Y := 'Y';
         Z := 'Z';
      end P5;

      procedure P6 (X : Boolean) is
      begin
         if X /= True then
            Failed ("WRONG VALUE FOR IN PARAMETER - NOT");
         end if;
      end P6;

      procedure P7 (X : Boolean) is
      begin
         if X /= True then
            Failed ("WRONG VALUE FOR IN PARAMETER - AND");
         end if;
      end P7;

      procedure P8 (X : Boolean) is
      begin
         if X /= True then
            Failed ("WRONG VALUE FOR IN PARAMETER - OR");
         end if;
      end P8;

      procedure P9 (X : Boolean) is
      begin
         if X /= True then
            Failed ("WRONG VALUE FOR IN PARAMETER - XOR");
         end if;
      end P9;

   begin

      if N1 (Ident_Int (2)) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - N1");
      end if;
      N1 (Ident_Int (2)) := 7;
      if N1 /= (1, 7, 3, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N1");
      end if;
      N1 := (1, 2, 3, 4);
      P1 (N1 (Ident_Int (2)), N1 (Ident_Int (3)), N1 (Ident_Int (1)), "N1");
      if N1 /= (9, 2, 8, 4) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N1");
      end if;

      if N2 (Ident_Int (3)) /= 3 then
         Failed ("WRONG VALUE FOR EXPRESSION - N2");
      end if;
      N2 (Ident_Int (3)) := 7;
      if N2.all /= (1, 2, 7, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N2");
      end if;
      N2.all := (2, 1, 4, 3);
      P1 (N2 (Ident_Int (1)), N2 (Ident_Int (4)), N2 (Ident_Int (2)), "N2");
      if N2.all /= (2, 9, 4, 8) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N2");
      end if;

      if "&" (String'("AB"), String'("CDEF")) (Ident_Int (5)) /=
        Character'('E')
      then
         Failed ("WRONG VALUE FOR EXPRESSION - '&'");
      end if;
      P2 ("&" ("AB", "CD") (Ident_Int (3)));

      if "NOT" (M2a) (Ident_Int (4)) /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - 'NOT'");
      end if;
      P6 ("NOT" (M2a) (Ident_Int (4)));

      if "AND" (M2a, M2b) (Ident_Int (3)) /= False then
         Failed ("WRONG VALUE FOR EXPRESSION - 'AND'");
      end if;
      P7 ("AND" (M2a, M2b) (Ident_Int (1)));

      if "OR" (M2a, M2b) (Ident_Int (3)) /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - 'OR'");
      end if;
      P8 ("OR" (M2a, M2b) (Ident_Int (3)));

      if "XOR" (M2a, M2b) (Ident_Int (1)) /= False then
         Failed ("WRONG VALUE FOR EXPRESSION - 'XOR'");
      end if;
      P9 ("XOR" (M2a, M2b) (Ident_Int (3)));

      if F1 (Ident_Int (3)) /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - F1");
      end if;
      P3 (F1 (Ident_Int (3)));

      N2 := new A1'(1, 2, 3, 4);
      if F2 (Ident_Int (2)) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - F2");
      end if;
      F2 (Ident_Int (3)) := 7;
      if N2.all /= (1, 2, 7, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - F2");
      end if;
      N2.all := (1, 2, 3, 4);
      P1 (F2 (Ident_Int (2)), F2 (Ident_Int (3)), F2 (Ident_Int (1)), "F2");
      if N2.all /= (9, 2, 8, 4) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
      end if;

      if N3 (2 .. 5) (Ident_Int (2)) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION (LOWER BOUND) - N3");
      end if;
      if N3 (2 .. 5) (Ident_Int (5)) /= 5 then
         Failed ("WRONG VALUE FOR EXPRESSION (UPPER BOUND) - N3");
      end if;
      N3 (2 .. 5) (Ident_Int (2)) := 8;
      if N3 /= (1, 8, 3, 4, 5, 6, 7) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N3");
      end if;
      N3 := (5, 3, 4, 2, 1, 6, 7);
      P1 (N3 (2 .. 5) (Ident_Int (4)),
         N3 (2 .. 5) (Ident_Int (2)),
         N3 (2 .. 5) (Ident_Int (5)),
         "N3");
      if N3 /= (5, 8, 4, 2, 9, 6, 7) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N3");
      end if;

      if N4 (1) (Ident_Int (2)) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - N4");
      end if;
      N4 (3) (Ident_Int (1)) := 20;
      if N4 /=
        ((1, 2, 3, 4), (5, 6, 7, 8), (20, 10, 11, 12), (13, 14, 15, 16))
      then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N4");
      end if;
      N4 :=
        (1 => (0, 6, 4, 2),
         2 => (10, 11, 12, 13),
         3 => (14, 15, 16, 17),
         4 => (7, 5, 3, 1));
      P1 (N4 (1) (Ident_Int (4)),
         N4 (4) (Ident_Int (3)),
         N4 (2) (Ident_Int (1)),
         "N4");
      if N4 /=
        ((0, 6, 4, 2), (9, 11, 12, 13), (14, 15, 16, 17), (7, 5, 8, 1))
      then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N4");
      end if;

      N1 := (1, 2, 3, 4);
      if C41103b.N1 (Ident_Int (2)) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - C41103B.N1");
      end if;
      C41103b.N1 (Ident_Int (2)) := 7;
      if N1 /= (1, 7, 3, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - C41103B.N1");
      end if;
      N1 := (1, 2, 3, 4);
      P1 (C41103b.N1 (Ident_Int (2)),
         C41103b.N1 (Ident_Int (3)),
         C41103b.N1 (Ident_Int (1)),
         "C41103B.N1");
      if N1 /= (9, 2, 8, 4) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER " & "- C41103B.N1");
      end if;

      if N5.S (Ident_Int (3)) /= 'C' then
         Failed ("WRONG VALUE FOR EXPRESSION - N5");
      end if;
      N5.S (Ident_Int (4)) := 'X';
      if N5.S /= "ABCX" then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N5");
      end if;
      N5.S := "ABCD";
      P5 (N5.S (Ident_Int (1)), N5.S (Ident_Int (4)), N5.S (Ident_Int (2)));
      if N5.S /= "AZCY" then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N5");
      end if;

      declare
         package P is
            type Lim is limited private;
            procedure Init (V : out Lim; X, Y, Z : Integer);
            procedure Assign (One : out Lim; Two : Lim);
            function "=" (One, Two : Lim) return Boolean;
         private
            type Lim is array (1 .. 3) of Integer;
         end P;

         use P;

         type A is array (1 .. 3) of Lim;

         N6 : Lim;

         package body P is
            procedure Init (V : out Lim; X, Y, Z : Integer) is
            begin
               V := (X, Y, Z);
            end Init;

            procedure Assign (One : out Lim; Two : Lim) is
            begin
               One := Two;
            end Assign;

            function "=" (One, Two : Lim) return Boolean is
            begin
               if One (1) = Two (1) and
                 One (2) = Two (2) and
                 One (3) = Two (3)
               then
                  return True;
               else
                  return False;
               end if;
            end "=";
         end P;

         function Fr return A is
         begin
            return Res : A do
               Init (Res (1), 1, 2, 3);
               Init (Res (2), 4, 5, 6);
               Init (Res (3), 7, 8, 9);
            end return;
         end Fr;

      begin
         Init (N6, 0, 0, 0);

         Assign (N6, Fr (2));

         if N6 /= Fr (2) then
            Failed ("WRONG VALUE FROM LIMITED COMPONENT TYPE");
         end if;

      end;
   end;

   Result;
end C41103b;

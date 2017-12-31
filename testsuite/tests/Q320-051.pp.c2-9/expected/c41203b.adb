-- C41203B.ADA

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
--     CHECK THAT THE NAME PART OF A SLICE MAY BE:
--        AN IDENTIFIER DENOTING A ONE DIMENSIONAL ARRAY OBJECT - N1;
--        AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE
--           DESIGNATES A ONE DIMENSIONAL ARRAY OBJECT - N2;
--        A FUNCTION CALL DELIVERING A ONE DIMENSIONAL ARRAY OBJECT
--           USING PREDEFINED FUNCTIONS - &, AND THE LOGICAL OPERATORS
--        A USER-DEFINED FUNCTION - F1;
--        A FUNCTION CALL DELIVERING AN ACCESS VALUE THAT
--           DESIGNATES A ONE DIMENSIONAL ARRAY - F2;
--        A SLICE - N3;
--        AN INDEXED COMPONENT DENOTING A ONE DIMENSIONAL ARRAY OBJECT
--           (ARRAY OF ARRAYS) - N4;
--        AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--           ENCLOSING ITS DECLARATION - C41203B.N1;
--        A RECORD COMPONENT (OF A RECORD CONTAINING ONE OR MORE
--           ARRAYS WHOSE BOUNDS DEPEND ON A DISCRIMINANT) - N5.
--     CHECK THAT THE APPROPRIATE SLICE IS ACCESSED (FOR
--     DYNAMIC INDICES).

-- HISTORY:
--     WKB 08/05/81  CREATED ORIGINAL TEST.
--     SPS 02/04/83
--     BCB 08/02/88  MODIFIED HEADER FORMAT AND ADDED CALLS TO THE
--                   LOGICAL OPERATORS.
--     BCB 04/16/90  ADDED TEST FOR PREFIX OF INDEXED COMPONENT HAVING
--                   A LIMITED TYPE.
--     PWN 11/30/94  SUBTYPE QUALIFIED LITERALS FOR ADA 9X.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.
--     RLB 08/17/07  ADDED MISSING CONSTRAINT TO PREVIOUS FIX.

with Report; use Report;
procedure C41203b is

   type T1 is array (Integer range <>) of Integer;
   subtype A1 is T1 (1 .. 6);
   N1 : A1 := (1, 2, 3, 4, 5, 6);

begin
   Test
     ("C41203B",
      "CHECK THAT THE NAME PART OF A SLICE MAY BE " &
      "OF CERTAIN FORMS AND THAT THE APPROPRIATE " &
      "SLICE IS ACCESSED (FOR DYNAMIC INDICES)");

   declare

      type T2 is array (Integer range <>) of Boolean;
      subtype A2 is T2 (1 .. 6);
      type A3 is access A1;
      type A4 is array (Integer range 1 .. 3) of A1;
      type R (Length : Integer) is record
         S : String (1 .. Length);
      end record;

      N2 : A3          := new A1'(1, 2, 3, 4, 5, 6);
      N3 : T1 (1 .. 7) := (1, 2, 3, 4, 5, 6, 7);
      N4 : A4          :=
        (1 => (1, 2, 3, 4, 5, 6), 2 => (7, 8, 9, 10, 11, 12),
         3 => (13, 14, 15, 16, 17, 18));
      N5 : R (6) := (Length => 6, S => "ABCDEF");

      M2a : A2 := (True, True, True, False, False, False);
      M2b : A2 := (True, False, True, False, True, False);

      function F1 return A2 is
      begin
         return (False, False, True, False, True, True);
      end F1;

      function F2 return A3 is
      begin
         return N2;
      end F2;

      procedure P1 (X : in T1; Y : in out T1; Z : out T1; W : in String) is
      begin
         if X /= (1, 2) then
            Failed ("WRONG VALUE FOR IN PARAMETER - " & W);
         end if;
         if Y /= (3, 4) then
            Failed ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
         end if;
         Y := (10, 11);
         Z := (12, 13);
      end P1;

      procedure P2 (X : String) is
      begin
         if X /= "BC" then
            Failed ("WRONG VALUE FOR IN PARAMETER - '&'");
         end if;
      end P2;

      procedure P3 (X : T2) is
      begin
         if X /= (False, True, False) then
            Failed ("WRONG VALUE FOR IN PARAMETER - F1");
         end if;
      end P3;

      procedure P5 (X : in String; Y : in out String; Z : out String) is
      begin
         if X /= "EF" then
            Failed ("WRONG VALUE FOR IN PARAMETER - N5");
         end if;
         if Y /= "CD" then
            Failed ("WRONG VALUE FOR IN OUT PARAMETER - N5");
         end if;
         Y := "XY";
         Z := "WZ";
      end P5;

      procedure P6 (X : T2) is
      begin
         if X /= (False, False, True) then
            Failed ("WRONG VALUE FOR IN PARAMETER - NOT");
         end if;
      end P6;

      procedure P7 (X : T2) is
      begin
         if X /= (False, True, False) then
            Failed ("WRONG VALUE FOR IN PARAMETER - AND");
         end if;
      end P7;

      procedure P8 (X : T2) is
      begin
         if X /= (False, True, False) then
            Failed ("WRONG VALUE FOR IN PARAMETER - OR");
         end if;
      end P8;

      procedure P9 (X : T2) is
      begin
         if X /= (False, True, False) then
            Failed ("WRONG VALUE FOR IN PARAMETER - XOR");
         end if;
      end P9;

   begin

      if N1 (Ident_Int (1) .. Ident_Int (2)) /= (1, 2) then
         Failed ("WRONG VALUE FOR EXPRESSION - N1");
      end if;
      N1 (Ident_Int (1) .. Ident_Int (2)) := (7, 8);
      if N1 /= (7, 8, 3, 4, 5, 6) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N1");
      end if;
      N1 := (1, 2, 3, 4, 5, 6);
      P1 (N1 (Ident_Int (1) .. Ident_Int (2)),
         N1 (Ident_Int (3) .. Ident_Int (4)),
         N1 (Ident_Int (5) .. Ident_Int (6)), "N1");
      if N1 /= (1, 2, 10, 11, 12, 13) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N1");
      end if;

      if N2 (Ident_Int (4) .. Ident_Int (6)) /= (4, 5, 6) then
         Failed ("WRONG VALUE FOR EXPRESSION - N2");
      end if;
      N2 (Ident_Int (4) .. Ident_Int (6)) := (7, 8, 9);
      if N2.all /= (1, 2, 3, 7, 8, 9) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N2");
      end if;
      N2.all := (1, 2, 5, 6, 3, 4);
      P1 (N2 (Ident_Int (1) .. Ident_Int (2)),
         N2 (Ident_Int (5) .. Ident_Int (6)),
         N2 (Ident_Int (3) .. Ident_Int (4)), "N2");
      if N2.all /= (1, 2, 12, 13, 10, 11) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N2");
      end if;

      if "&" (String'("AB"), String'("CDEF"))
          (Ident_Int (4) .. Ident_Int (6)) /=
        String'("DEF") then
         Failed ("WRONG VALUE FOR EXPRESSION - '&'");
      end if;
      P2 ("&" ("AB", "CD") (Ident_Int (2) .. Ident_Int (3)));

      if "NOT" (M2a) (Ident_Int (3) .. Ident_Int (5)) /= (False, True, True)
      then
         Failed ("WRONG VALUE FOR EXPRESSION - 'NOT'");
      end if;
      P6 ("NOT" (M2a) (Ident_Int (2) .. Ident_Int (4)));

      if "AND" (M2a, M2b) (Ident_Int (3) .. Ident_Int (5)) /=
        (True, False, False) then
         Failed ("WRONG VALUE FOR EXPRESSION - 'AND'");
      end if;
      P7 ("AND" (M2a, M2b) (Ident_Int (2) .. Ident_Int (4)));

      if "OR" (M2a, M2b) (Ident_Int (3) .. Ident_Int (5)) /=
        (True, False, True) then
         Failed ("WRONG VALUE FOR EXPRESSION - 'OR'");
      end if;
      P8 ("OR" (M2a, M2b) (Ident_Int (4) .. Ident_Int (6)));

      if "XOR" (M2a, M2b) (Ident_Int (3) .. Ident_Int (5)) /=
        (False, False, True) then
         Failed ("WRONG VALUE FOR EXPRESSION - 'XOR'");
      end if;
      P9 ("XOR" (M2a, M2b) (Ident_Int (1) .. Ident_Int (3)));

      if F1 (Ident_Int (1) .. Ident_Int (2)) /= (False, False) then
         Failed ("WRONG VALUE FOR EXPRESSION - F1");
      end if;
      P3 (F1 (Ident_Int (2) .. Ident_Int (4)));

      N2 := new A1'(1, 2, 3, 4, 5, 6);
      if F2 (Ident_Int (2) .. Ident_Int (6)) /= (2, 3, 4, 5, 6) then
         Failed ("WRONG VALUE FOR EXPRESSION - F2");
      end if;
      F2 (Ident_Int (3) .. Ident_Int (3)) := (5 => 7);
      if N2.all /= (1, 2, 7, 4, 5, 6) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - F2");
      end if;
      N2.all := (5, 6, 1, 2, 3, 4);
      P1 (F2 (Ident_Int (3) .. Ident_Int (4)),
         F2 (Ident_Int (5) .. Ident_Int (6)),
         F2 (Ident_Int (1) .. Ident_Int (2)), "F2");
      if N2.all /= (12, 13, 1, 2, 10, 11) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
      end if;

      if N3 (2 .. 7) (Ident_Int (2) .. Ident_Int (4)) /= (2, 3, 4) then
         Failed ("WRONG VALUE FOR EXPRESSION - N3");
      end if;
      N3 (2 .. 7) (Ident_Int (4) .. Ident_Int (5)) := (8, 9);
      if N3 /= (1, 2, 3, 8, 9, 6, 7) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N3");
      end if;
      N3 := (5, 3, 4, 1, 2, 6, 7);
      P1 (N3 (2 .. 7) (Ident_Int (4) .. Ident_Int (5)),
         N3 (2 .. 7) (Ident_Int (2) .. Ident_Int (3)),
         N3 (2 .. 7) (Ident_Int (6) .. Ident_Int (7)), "N3");
      if N3 /= (5, 10, 11, 1, 2, 12, 13) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N3");
      end if;

      if N4 (1) (Ident_Int (3) .. Ident_Int (5)) /= (3, 4, 5) then
         Failed ("WRONG VALUE FOR EXPRESSION - N4");
      end if;
      N4 (2) (Ident_Int (1) .. Ident_Int (3)) := (21, 22, 23);
      if N4 /=
        ((1, 2, 3, 4, 5, 6), (21, 22, 23, 10, 11, 12),
         (13, 14, 15, 16, 17, 18))
      then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N4");
      end if;
      N4 :=
        (1 => (18, 19, 20, 21, 22, 23), 2 => (17, 16, 15, 1, 2, 14),
         3 => (7, 3, 4, 5, 6, 8));
      P1 (N4 (2) (Ident_Int (4) .. Ident_Int (5)),
         N4 (3) (Ident_Int (2) .. Ident_Int (3)),
         N4 (1) (Ident_Int (5) .. Ident_Int (6)), "N4");
      if N4 /=
        ((18, 19, 20, 21, 12, 13), (17, 16, 15, 1, 2, 14),
         (7, 10, 11, 5, 6, 8))
      then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N4");
      end if;

      N1 := (1, 2, 3, 4, 5, 6);
      if C41203b.N1 (Ident_Int (1) .. Ident_Int (2)) /= (1, 2) then
         Failed ("WRONG VALUE FOR EXPRESSION - C41203B.N1");
      end if;
      C41203b.N1 (Ident_Int (1) .. Ident_Int (2)) := (7, 8);
      if N1 /= (7, 8, 3, 4, 5, 6) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - C41203B.N1");
      end if;
      N1 := (1, 2, 3, 4, 5, 6);
      P1 (C41203b.N1 (Ident_Int (1) .. Ident_Int (2)),
         C41203b.N1 (Ident_Int (3) .. Ident_Int (4)),
         C41203b.N1 (Ident_Int (5) .. Ident_Int (6)), "C41203B.N1");
      if N1 /= (1, 2, 10, 11, 12, 13) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER " & "- C41203B.N1");
      end if;

      if N5.S (Ident_Int (1) .. Ident_Int (5)) /= "ABCDE" then
         Failed ("WRONG VALUE FOR EXPRESSION - N5");
      end if;
      N5.S (Ident_Int (4) .. Ident_Int (6)) := "PQR";
      if N5.S /= "ABCPQR" then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N5");
      end if;
      N5.S := "ABCDEF";
      P5 (N5.S (Ident_Int (5) .. Ident_Int (6)),
         N5.S (Ident_Int (3) .. Ident_Int (4)),
         N5.S (Ident_Int (1) .. Ident_Int (2)));
      if N5.S /= "WZXYEF" then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N5");
      end if;

      declare
         package P is
            type Lim is limited private;
            type A is array (Integer range <>) of Lim;
            procedure Init (V : out Lim; X, Y, Z : Integer);
            procedure Assign (One : out Lim; Two : Lim);
            function "=" (One, Two : A) return Boolean;
         private
            type Lim is array (1 .. 3) of Integer;
         end P;

         use P;

         H : A (1 .. 5);

         N6 : A (1 .. 3);

         package body P is
            procedure Init (V : out Lim; X, Y, Z : Integer) is
            begin
               V := (X, Y, Z);
            end Init;

            procedure Assign (One : out Lim; Two : Lim) is
            begin
               One := Two;
            end Assign;

            function "=" (One, Two : A) return Boolean is
            begin
               if One (1) = Two (2) and One (2) = Two (3) and One (3) = Two (4)
               then
                  return True;
               else
                  return False;
               end if;
            end "=";
         end P;

         function Fr return A is
         begin
            return Res : A (1 .. 5) do
               Init (Res (1), 1, 2, 3);
               Init (Res (2), 4, 5, 6);
               Init (Res (3), 7, 8, 9);
               Init (Res (4), 10, 11, 12);
               Init (Res (5), 13, 14, 15);
            end return;
         end Fr;

      begin
         Init (H (1), 1, 2, 3);
         Init (H (2), 4, 5, 6);
         Init (H (3), 7, 8, 9);
         Init (H (4), 10, 11, 12);
         Init (H (5), 13, 14, 15);
         Init (N6 (1), 0, 0, 0);
         Init (N6 (2), 0, 0, 0);
         Init (N6 (3), 0, 0, 0);

         Assign (N6 (1), H (2));
         Assign (N6 (2), H (3));
         Assign (N6 (3), H (4));

         if N6 /= Fr (2 .. 4) then
            Failed ("WRONG VALUE FROM LIMITED COMPONENT TYPE");
         end if;
      end;
   end;

   Result;
end C41203b;

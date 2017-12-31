-- C41203A.ADA

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
-- CHECK THAT THE NAME PART OF A SLICE MAY BE:
--   AN IDENTIFIER DENOTING A ONE DIMENSIONAL ARRAY OBJECT - N1;
--   AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE
--        DESIGNATES A ONE DIMENSIONAL ARRAY OBJECT - N2;
--   A FUNCTION CALL DELIVERING A ONE DIMENSIONAL ARRAY OBJECT USING
--        A PREDEFINED FUNCTION - &,
--        A USER-DEFINED FUNCTION - F1;
--   A FUNCTION CALL DELIVERING AN ACCESS VALUE THAT
--        DESIGNATES A ONE DIMENSIONAL ARRAY - F2;
--   A SLICE - N3;
--   AN INDEXED COMPONENT DENOTING A ONE DIMENSIONAL ARRAY OBJECT
--        (ARRAY OF ARRAYS) - N4;
--   AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--        ENCLOSING ITS DECLARATION - C41203A.N1;
--   A RECORD COMPONENT (OF A RECORD CONTAINING ONE OR MORE
--        ARRAYS WHOSE BOUNDS DEPEND ON A DISCRIMINANT) - N5.
-- CHECK THAT THE APPROPRIATE SLICE IS ACCESSED (FOR
--   STATIC INDICES).

-- WKB 8/5/81
-- SPS 11/1/82
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

with Report; use Report;
procedure C41203a is

   type T1 is array (Integer range <>) of Integer;
   subtype A1 is T1 (1 .. 6);
   N1 : A1 := (1, 2, 3, 4, 5, 6);

begin
   Test
     ("C41203A",
      "CHECK THAT THE NAME PART OF A SLICE MAY BE " &
      "OF CERTAIN FORMS AND THAT THE APPROPRIATE " &
      "SLICE IS ACCESSED (FOR STATIC INDICES)");

   declare

      type T2 is array (Integer range <>) of Boolean;
      subtype A2 is T2 (1 .. 6);
      type A3 is access A1;
      subtype Si is Integer range 1 .. 3;
      type A4 is array (Si) of A1;
      type R (Length : Integer) is record
         S : String (1 .. Length);
      end record;

      N2 : A3          := new A1'(1, 2, 3, 4, 5, 6);
      N3 : T1 (1 .. 7) := (1, 2, 3, 4, 5, 6, 7);
      N4 : A4          :=
        (1 => (1, 2, 3, 4, 5, 6), 2 => (7, 8, 9, 10, 11, 12),
         3 => (13, 14, 15, 16, 17, 18));
      N5 : R (6) := (Length => 6, S => "ABCDEF");

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

   begin

      if N1 (1 .. 2) /= (1, 2) then
         Failed ("WRONG VALUE FOR EXPRESSION - N1");
      end if;
      N1 (1 .. 2) := (7, 8);
      if N1 /= (7, 8, 3, 4, 5, 6) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N1");
      end if;
      N1 := (1, 2, 3, 4, 5, 6);
      P1 (N1 (1 .. 2), N1 (3 .. 4), N1 (5 .. 6), "N1");
      if N1 /= (1, 2, 10, 11, 12, 13) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N1");
      end if;

      if N2 (4 .. 6) /= (4, 5, 6) then
         Failed ("WRONG VALUE FOR EXPRESSION - N2");
      end if;
      N2 (4 .. 6) := (7, 8, 9);
      if N2.all /= (1, 2, 3, 7, 8, 9) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N2");
      end if;
      N2.all := (1, 2, 5, 6, 3, 4);
      P1 (N2 (1 .. 2), N2 (5 .. 6), N2 (3 .. 4), "N2");
      if N2.all /= (1, 2, 12, 13, 10, 11) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N2");
      end if;

      if "&" (String'("AB"), String'("CDEF")) (4 .. 6) /= String'("DEF") then
         Failed ("WRONG VALUE FOR EXPRESSION - '&'");
      end if;
      P2 ("&" ("AB", "CD") (2 .. 3));

      if F1 (1 .. 2) /= (False, False) then
         Failed ("WRONG VALUE FOR EXPRESSION - F1");
      end if;
      P3 (F1 (2 .. 4));

      N2 := new A1'(1, 2, 3, 4, 5, 6);
      if F2 (2 .. 6) /= (2, 3, 4, 5, 6) then
         Failed ("WRONG VALUE FOR EXPRESSION - F2");
      end if;
      F2 (3 .. 3) := (5 => 7);
      if N2.all /= (1, 2, 7, 4, 5, 6) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - F2");
      end if;
      N2.all := (5, 6, 1, 2, 3, 4);
      P1 (F2 (3 .. 4), F2 (5 .. 6), F2 (1 .. 2), "F2");
      if N2.all /= (12, 13, 1, 2, 10, 11) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
      end if;

      if N3 (2 .. 7) (2 .. 4) /= (2, 3, 4) then
         Failed ("WRONG VALUE FOR EXPRESSION - N3");
      end if;
      N3 (2 .. 7) (4 .. 5) := (8, 9);
      if N3 /= (1, 2, 3, 8, 9, 6, 7) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N3");
      end if;
      N3 := (5, 3, 4, 1, 2, 6, 7);
      P1 (N3 (2 .. 7) (4 .. 5), N3 (2 .. 7) (2 .. 3), N3 (2 .. 7) (6 .. 7),
         "N3");
      if N3 /= (5, 10, 11, 1, 2, 12, 13) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N3");
      end if;

      if N4 (1) (3 .. 5) /= (3, 4, 5) then
         Failed ("WRONG VALUE FOR EXPRESSION - N4");
      end if;
      N4 (2) (1 .. 3) := (21, 22, 23);
      if N4 /=
        ((1, 2, 3, 4, 5, 6), (21, 22, 23, 10, 11, 12),
         (13, 14, 15, 16, 17, 18))
      then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N4");
      end if;
      N4 :=
        (1 => (18, 19, 20, 21, 22, 23), 2 => (17, 16, 15, 1, 2, 14),
         3 => (7, 3, 4, 5, 6, 8));
      P1 (N4 (2) (4 .. 5), N4 (3) (2 .. 3), N4 (1) (5 .. 6), "N4");
      if N4 /=
        ((18, 19, 20, 21, 12, 13), (17, 16, 15, 1, 2, 14),
         (7, 10, 11, 5, 6, 8))
      then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N4");
      end if;

      N1 := (1, 2, 3, 4, 5, 6);
      if C41203a.N1 (1 .. 2) /= (1, 2) then
         Failed ("WRONG VALUE FOR EXPRESSION - C41203A.N1");
      end if;
      C41203a.N1 (1 .. 2) := (7, 8);
      if N1 /= (7, 8, 3, 4, 5, 6) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - C41203A.N1");
      end if;
      N1 := (1, 2, 3, 4, 5, 6);
      P1 (C41203a.N1 (1 .. 2), C41203a.N1 (3 .. 4), C41203a.N1 (5 .. 6),
         "C41203A.N1");
      if N1 /= (1, 2, 10, 11, 12, 13) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER " & "- C41203A.N1");
      end if;

      if N5.S (1 .. 5) /= "ABCDE" then
         Failed ("WRONG VALUE FOR EXPRESSION - N5");
      end if;
      N5.S (4 .. 6) := "PQR";
      if N5.S /= "ABCPQR" then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N5");
      end if;
      N5.S := "ABCDEF";
      P5 (N5.S (5 .. 6), N5.S (3 .. 4), N5.S (1 .. 2));
      if N5.S /= "WZXYEF" then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N5");
      end if;
   end;

   Result;
end C41203a;

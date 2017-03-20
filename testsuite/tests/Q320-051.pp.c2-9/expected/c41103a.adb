-- C41103A.ADA

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
-- CHECK THAT THE NAME IN AN INDEXED_COMPONENT MAY BE:
--   AN IDENTIFIER DENOTING AN ARRAY OBJECT - N1;
--   AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE
--        DESIGNATES AN ARRAY OBJECT - N2;
--   A FUNCTION CALL DELIVERING AN ARRAY OBJECT USING
--        A PREDEFINED FUNCTION - &,
--        A USER-DEFINED FUNCTION - F1;
--   A FUNCTION CALL DELIVERING AN ACCESS VALUE THAT
--        DESIGNATES AN ARRAY - F2;
--   A SLICE (CHECKING UPPER AND LOWER BOUND COMPONENTS) - N3;
--   AN INDEXED COMPONENT DENOTING AN ARRAY OBJECT
--        (ARRAY OF ARRAYS) - N4;
--   AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--        ENCLOSING ITS DECLARATION - C41103A.N1;
--   A RECORD COMPONENT (OF A RECORD CONTAINING ONE OR MORE
--        ARRAYS WHOSE BOUNDS DEPEND ON A DISCRIMINANT) - N5.
-- CHECK THAT THE APPROPRIATE COMPONENT IS ACCESSED (FOR
--   STATIC INDICES).

-- WKB 7/27/81
-- JRK 7/28/81
-- SPS 10/26/82
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

with Report; use Report;
procedure C41103a is

   type A1 is array (Integer range 1 .. 4) of Integer;
   N1 : A1 := (1, 2, 3, 4);

begin
   Test
     ("C41103A",
      "CHECK THAT AN INDEXED_COMPONENT MAY BE OF " &
      "CERTAIN FORMS AND THAT THE APPROPRIATE " &
      "COMPONENT IS ACCESSED (FOR STATIC INDICES)");

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

   begin

      if N1 (2) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - N1");
      end if;
      N1 (2) := 7;
      if N1 /= (1, 7, 3, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N1");
      end if;
      N1 := (1, 2, 3, 4);
      P1 (N1 (2), N1 (3), N1 (1), "N1");
      if N1 /= (9, 2, 8, 4) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N1");
      end if;

      if N2 (3) /= 3 then
         Failed ("WRONG VALUE FOR EXPRESSION - N2");
      end if;
      N2 (3) := 7;
      if N2.all /= (1, 2, 7, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N2");
      end if;
      N2.all := (2, 1, 4, 3);
      P1 (N2 (1), N2 (4), N2 (2), "N2");
      if N2.all /= (2, 9, 4, 8) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N2");
      end if;

      if "&" (String'("AB"), String'("CDEF")) (5) /= Character'('E') then
         Failed ("WRONG VALUE FOR EXPRESSION - '&'");
      end if;
      P2 ("&" ("AB", "CD") (3));

      if F1 (3) /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - F1");
      end if;
      P3 (F1 (3));

      N2 := new A1'(1, 2, 3, 4);
      if F2 (2) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - F2");
      end if;
      F2 (3) := 7;
      if N2.all /= (1, 2, 7, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - F2");
      end if;
      N2.all := (1, 2, 3, 4);
      P1 (F2 (2), F2 (3), F2 (1), "F2");
      if N2.all /= (9, 2, 8, 4) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
      end if;

      if N3 (2 .. 5) (5) /= 5 then
         Failed ("WRONG VALUE FOR EXPRESSION - N3");
      end if;
      N3 (2 .. 5) (2) := 8;
      if N3 /= (1, 8, 3, 4, 5, 6, 7) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N3");
      end if;
      N3 := (5, 3, 4, 2, 1, 6, 7);
      P1 (N3 (2 .. 5) (4), N3 (2 .. 5) (2), N3 (2 .. 5) (5), "N3");
      if N3 /= (5, 8, 4, 2, 9, 6, 7) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N3");
      end if;

      if N4 (1) (2) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - N4");
      end if;
      N4 (3) (1) := 20;
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
      P1 (N4 (1) (4), N4 (4) (3), N4 (2) (1), "N4");
      if N4 /=
        ((0, 6, 4, 2), (9, 11, 12, 13), (14, 15, 16, 17), (7, 5, 8, 1))
      then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N4");
      end if;

      N1 := (1, 2, 3, 4);
      if C41103a.N1 (2) /= 2 then
         Failed ("WRONG VALUE FOR EXPRESSION - C41103A.N1");
      end if;
      C41103a.N1 (2) := 7;
      if N1 /= (1, 7, 3, 4) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - C41103A.N1");
      end if;
      N1 := (1, 2, 3, 4);
      P1 (C41103a.N1 (2), C41103a.N1 (3), C41103a.N1 (1), "C41103A.N1");
      if N1 /= (9, 2, 8, 4) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER " & "- C41103A.N1");
      end if;

      if N5.S (3) /= 'C' then
         Failed ("WRONG VALUE FOR EXPRESSION - N5");
      end if;
      N5.S (4) := 'X';
      if N5.S /= "ABCX" then
         Failed ("WRONG TARGET FOR ASSIGNMENT - N5");
      end if;
      N5.S := "ABCD";
      P5 (N5.S (1), N5.S (4), N5.S (2));
      if N5.S /= "AZCY" then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - N5");
      end if;
   end;

   Result;
end C41103a;

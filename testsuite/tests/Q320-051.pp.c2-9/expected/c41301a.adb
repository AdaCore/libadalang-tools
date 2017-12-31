-- C41301A.ADA

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
-- CHECK THAT THE NOTATION L.R MAY BE USED TO DENOTE A RECORD COMPONENT,
--   WHERE R IS THE IDENTIFIER OF SUCH COMPONENT, AND L MAY BE ANY OF
--   THE FOLLOWING:
--        AN IDENTIFIER DENOTING A RECORD OBJECT - X2;
--        AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE DESIGNATES
--           A RECORD OBJECT - X3;
--        A FUNCTION CALL DELIVERING A RECORD VALUE - F1;
--        A FUNCTION CALL DELIVERING AN ACCESS VALUE DESIGNATING A
--           RECORD OBJECT - F2;
--        AN INDEXED COMPONENT - X4;
--        AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--           ENCLOSING THE IDENTIFIER'S DECLARATION - C41301A.X1;
--        A SELECTED COMPONENT DENOTING A RECORD (WHICH IS A COMPONENT
--           OF ANOTHER RECORD) - X5.

-- WKB 8/13/81
-- JRK 8/17/81
-- SPS 10/26/82

with Report; use Report;
procedure C41301a is

   type T1 is record
      A : Integer;
      B : Boolean;
      C : Boolean;
   end record;
   X1 : T1 := (A => 1, B => True, C => False);

begin
   Test
     ("C41301A",
      "CHECK THAT THE NOTATION L.R MAY BE USED TO " &
      "DENOTE A RECORD COMPONENT, WHERE R IS THE " &
      "IDENTIFIER AND L MAY BE OF CERTAIN FORMS");

   declare

      type T2 (Disc : Integer := 0) is record
         D : Boolean;
         E : Integer;
         F : Boolean;
         case Disc is
            when 1 =>
               G : Boolean;
            when 2 =>
               H : Integer;
            when others =>
               null;
         end case;
      end record;
      X2 : T2 (2) := (Disc => 2, D => True, E => 3, F => False, H => 1);

      type T3 is access T1;
      X3 : T3 := new T1'(A => 1, B => True, C => False);

      type T4 is array (1 .. 3) of T1;
      X4 : T4 :=
        (1 => (1, True, False), 2 => (2, False, True), 3 => (3, True, False));

      type T5 is record
         I : Integer;
         J : T1;
      end record;
      X5 : T5 := (I => 5, J => (6, False, True));

      function F1 return T2 is
      begin
         return (Disc => 1, D => False, E => 3, F => True, G => False);
      end F1;

      function F2 return T3 is
      begin
         return X3;
      end F2;

      procedure P1 (X : in Boolean; Y : in out Integer; Z : out Boolean;
         W            :    String)
      is
      begin
         if X /= True then
            Failed ("WRONG VALUE FOR IN PARAMETER - " & W);
         end if;
         if Y /= 1 then
            Failed ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
         end if;
         Y := 10;
         Z := True;
      end P1;

      procedure P2 (X : in Integer) is
      begin
         if X /= 1 then
            Failed ("WRONG VALUE FOR IN PARAMETER - F1");
         end if;
      end P2;

   begin

      if X2.E /= 3 then
         Failed ("WRONG VALUE FOR EXPRESSION - X2");
      end if;
      X2.E := 5;
      if X2 /= (2, True, 5, False, 1) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - X2");
      end if;
      X2 := (Disc => 2, D => True, E => 3, F => False, H => 1);
      P1 (X2.D, X2.H, X2.F, "X2");
      if X2 /= (2, True, 3, True, 10) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - X2");
      end if;

      if X3.C /= False then
         Failed ("WRONG VALUE FOR EXPRESSION - X3");
      end if;
      X3.A := 5;
      if X3.all /= (5, True, False) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - X3");
      end if;
      X3 := new T1'(A => 1, B => True, C => False);
      P1 (X3.B, X3.A, X3.C, "X3");
      if X3.all /= (10, True, True) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - X3");
      end if;

      if F1.G /= False then
         Failed ("WRONG VALUE FOR EXPRESSION - F1");
      end if;
      P2 (F1.Disc);

      X3 := new T1'(A => 3, B => False, C => True);
      if F2.B /= False then
         Failed ("WRONG VALUE FOR EXPRESSION - F2");
      end if;
      F2.A := 4;
      if X3.all /= (4, False, True) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - F2");
      end if;
      X3 := new T1'(A => 1, B => False, C => True);
      P1 (F2.C, F2.A, F2.B, "F2");
      if X3.all /= (10, True, True) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
      end if;

      if X4 (2).C /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - X4");
      end if;
      X4 (3).A := 4;
      if X4 /= ((1, True, False), (2, False, True), (4, True, False)) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - X4");
      end if;
      X4 :=
        (1 => (2, True, False), 2 => (1, False, True), 3 => (3, True, False));
      P1 (X4 (3).B, X4 (2).A, X4 (1).C, "X4");
      if X4 /= ((2, True, True), (10, False, True), (3, True, False)) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - X4");
      end if;

      X1 := (A => 1, B => False, C => True);
      if C41301a.X1.C /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - C41301A.X1");
      end if;
      C41301a.X1.B := True;
      if X1 /= (1, True, True) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - C41301A.X1");
      end if;
      X1 := (A => 1, B => False, C => True);
      P1 (C41301a.X1.C, C41301a.X1.A, C41301a.X1.B, "C41301A.X1");
      if X1 /= (10, True, True) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - " & "C41301A.X1");
      end if;

      if X5.J.C /= True then
         Failed ("WRONG VALUE FOR EXPRESSION - X5");
      end if;
      X5.J.C := False;
      if X5 /= (5, (6, False, False)) then
         Failed ("WRONG TARGET FOR ASSIGNMENT - X5");
      end if;
      X5 := (I => 5, J => (A => 1, B => True, C => False));
      P1 (X5.J.B, X5.J.A, X5.J.C, "X5");
      if X5 /= (5, (10, True, True)) then
         Failed ("WRONG TARGET FOR (IN) OUT PARAMETER - X5");
      end if;

   end;

   Result;
end C41301a;

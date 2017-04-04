-- CC1308A.ADA

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
--     CHECK THAT FORMAL SUBPROGRAM PARAMETERS MAY OVERLOAD EACH OTHER
--     AND OTHER VISIBLE SUBPROGRAMS AND ENUMERATION LITERALS WITHIN AND
--     OUTSIDE OF THE GENERIC UNIT.

-- HISTORY:
--     DAT 09/08/81  CREATED ORIGINAL TEST.
--     SPS 10/26/82
--     SPS 02/09/83
--     BCB 08/09/88  REPLACED THE OLD TEST WITH A VERSION BASED ON
--                   AIG 6.6/T2.

with Report; use Report;

procedure Cc1308a is

   type Enum is (F1, F2, F3, F4, F5, F6, F7);

   function F1 (X : Integer) return Integer is
   begin
      return 2 * X;
   end F1;

   procedure F1 (X : in out Integer) is
   begin
      X := 3 * X;
   end F1;

   procedure F2 (Y : in out Integer; Z : in out Boolean) is
   begin
      Y := 2 * Y;
      Z := not Z;
   end F2;

   procedure F2 (Y : in out Integer) is
   begin
      Y := 3 * Y;
   end F2;

   procedure F3 (B : Boolean := False; A : in out Integer) is
   begin
      A := 2 * A;
   end F3;

   procedure F3 (A : in out Integer) is
   begin
      A := 3 * A;
   end F3;

   procedure F4 (C : in out Integer) is
   begin
      C := 2 * C;
   end F4;

   procedure F4 (C : in out Boolean) is
   begin
      C := not C;
   end F4;

   procedure F5 (D : in out Integer; E : in out Boolean) is
   begin
      D := 2 * D;
      E := not E;
   end F5;

   procedure F5 (E : in out Boolean; D : in out Integer) is
   begin
      E := not E;
      D := 3 * D;
   end F5;

   function F6 (G : Integer) return Integer is
   begin
      return 2 * G;
   end F6;

   function F6 (G : Integer) return Boolean is
   begin
      return True;
   end F6;

   function F7 return Integer is
   begin
      return 25;
   end F7;

   function F7 return Boolean is
   begin
      return False;
   end F7;

begin
   Test
     ("CC1308A",
      "CHECK THAT FORMAL SUBPROGRAM PARAMETERS MAY " &
      "OVERLOAD EACH OTHER AND OTHER VISIBLE " &
      "SUBPROGRAMS AND ENUMERATION LITERALS WITHIN " &
      "AND OUTSIDE OF THE GENERIC UNIT");

   declare
      generic
         with function F1 (X : Integer) return Integer;
         with procedure F1 (X : in out Integer);

         with procedure F2 (Y : in out Integer; Z : in out Boolean);
         with procedure F2 (Y : in out Integer);

         with procedure F3 (B : Boolean := False; A : in out Integer);
         with procedure F3 (A : in out Integer);

         with procedure F4 (C : in out Integer);
         with procedure F4 (C : in out Boolean);

         with procedure F5 (D : in out Integer; E : in out Boolean);
         with procedure F5 (E : in out Boolean; D : in out Integer);

         with function F6 (G : Integer) return Integer;
         with function F6 (G : Integer) return Boolean;

         with function F7 return Integer;
         with function F7 return Boolean;
      package P is
         type En is (F1, F2, F3, F4, F5, F6, F7);
      end P;

      package body P is
         X1, X2, Y1, Y2, A1, A2, C1, D1, D2, G1 : Integer := Ident_Int (5);

         Val : Integer := Ident_Int (0);

         Z1, B1, C2, E1, E2, Bool : Boolean := Ident_Bool (False);
      begin
         Val := F1 (X1);

         if not Equal (Val, 10) then
            Failed ("CASE 1 - WRONG VALUE RETURNED FROM " & "FUNCTION");
         end if;

         F1 (X2);

         if not Equal (X2, 15) then
            Failed ("CASE 1 - WRONG VALUE ASSIGNED INSIDE " & "PROCEDURE");
         end if;

         F2 (Y1, Z1);

         if not Equal (Y1, 10) or Z1 /= True then
            Failed ("CASE 2 - WRONG VALUES ASSIGNED INSIDE " & "PROCEDURE");
         end if;

         F2 (Y2);

         if not Equal (Y2, 15) then
            Failed ("CASE 2 - WRONG VALUE ASSIGNED INSIDE " & "PROCEDURE");
         end if;

         F3 (B1, A1);

         if not Equal (A1, 10) or B1 /= False then
            Failed ("CASE 3 - WRONG VALUES ASSIGNED INSIDE " & "PROCEDURE");
         end if;

         F3 (A2);

         if not Equal (A2, 15) then
            Failed ("CASE 3 - WRONG VALUE ASSIGNED INSIDE " & "PROCEDURE");
         end if;

         F4 (C1);

         if not Equal (C1, 10) then
            Failed
              ("CASE 4 - WRONG VALUE ASSIGNED INSIDE " &
               "PROCEDURE - BASE TYPE INTEGER");
         end if;

         F4 (C2);

         if C2 /= True then
            Failed
              ("CASE 4 - WRONG VALUE ASSIGNED INSIDE " &
               "PROCEDURE - BASE TYPE BOOLEAN");
         end if;

         F5 (D1, E1);

         if not Equal (D1, 10) or E1 /= True then
            Failed
              ("CASE 5 - WRONG VALUES ASSIGNED INSIDE " &
               "PROCEDURE - ORDER WAS INTEGER, BOOLEAN");
         end if;

         F5 (E2, D2);

         if E2 /= True or not Equal (D2, 15) then
            Failed
              ("CASE 5 - WRONG VALUES ASSIGNED INSIDE " &
               "PROCEDURE - ORDER WAS BOOLEAN, INTEGER");
         end if;

         Val := F6 (G1);

         if not Equal (Val, 10) then
            Failed
              ("CASE 6 - WRONG VALUE RETURNED FROM " &
               "FUNCTION - TYPE INTEGER");
         end if;

         Bool := F6 (G1);

         if Bool /= True then
            Failed
              ("CASE 6 - WRONG VALUE RETURNED FROM " &
               "FUNCTION - TYPE BOOLEAN");
         end if;

         Val := F7;

         if not Equal (Val, 25) then
            Failed
              ("CASE 7 - WRONG VALUE RETURNED FROM " &
               "PARAMETERLESS FUNCTION - TYPE INTEGER");
         end if;

         Bool := F7;

         if Bool /= False then
            Failed
              ("CASE 7 - WRONG VALUE RETURNED FROM " &
               "PARAMETERLESS FUNCTION - TYPE BOOLEAN");
         end if;
      end P;

      package New_P is new P
        (F1,
         F1,
         F2,
         F2,
         F3,
         F3,
         F4,
         F4,
         F5,
         F5,
         F6,
         F6,
         F7,
         F7);
   begin
      null;
   end;

   Result;
end Cc1308a;

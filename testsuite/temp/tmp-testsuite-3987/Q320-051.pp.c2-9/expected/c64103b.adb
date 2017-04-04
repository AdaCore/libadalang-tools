-- C64103B.ADA

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
--     CHECK THAT, FOR IN-OUT PARAMETERS OF A SCALAR TYPE,
--     CONSTRAINT_ERROR IS RAISED:
--          BEFORE A SUBPROGRAM CALL WHEN THE CONVERTED ACTUAL
--          PARAMETER IS OUTSIDE THE RANGE OF THE FORMAL PARAMETER'S
--          SUBTYPE;
--          AFTER A SUBPROGRAM CALL WHEN THE CONVERTED FORMAL PARAMETER
--          IS OUTSIDE THE RANGE OF THE ACTUAL PARAMETER'S SUBTYPE.

-- HISTORY:
--     CPP  07/18/84  CREATED ORIGINAL TEST.
--     VCL  10/27/87  MODIFIED THIS HEADER; ADDED STATEMENTS WHICH
--                    REFERENCED THE ACTUAL PARAMETERS IN THE SECOND
--                    SUBTEST.

with Report; use Report;
procedure C64103b is
begin
   Test
     ("C64103B",
      "FOR IN-OUT PARAMETERS OF A SCALAR TYPE, " &
      "CONSTRAINT_ERROR IS RAISED:  BEFORE A " &
      "SUBPROGRAM CALL WHEN THE CONVERTED ACTUAL " &
      "PARAMETER IS OUTSIDE THE RANGE OF THE FORMAL " &
      "PARAMETER'S SUBTYPE;  AFTER A SUBPROGRAM " &
      "CALL WHEN THE CONVERTED FORMAL PARAMETER IS " &
      "OUTSIDE THE RANGE OF THE ACTUAL PARAMETER'S " &
      "SUBTYPE");

   declare
      A0 : Integer := -9;
      A1 : Integer := Ident_Int (-1);
      type Subint is range -8 .. -2;

      type Float_Type is digits 3 range 0.0 .. 3.0;
      A2 : Float_Type := 0.12;
      A3 : Float_Type := 2.5;
      type New_Float is digits 3 range 1.0 .. 2.0;

      type Fixed_Type is delta 1.0 range -2.0 .. 5.0;
      A4 : Fixed_Type := -2.0;
      A5 : Fixed_Type := 4.0;
      type New_Fixed is delta 1.0 range -1.0 .. 3.0;

      A6 : Character := 'A';
      subtype Super_Char is Character range 'B' .. 'Q';

      type Color is (Red, Burgundy, Lilac, Maroon, Magenta);
      subtype A_Color is Color range Red .. Lilac;
      subtype B_Color is Color range Maroon .. Magenta;
      A7 : B_Color := Maroon;

      procedure P1 (X : in out Subint; S : String) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P1 (A" & S & ")");
      end P1;

      procedure P2 (X : in out New_Float; S : String) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P2 (A" & S & ")");
      end P2;

      procedure P3 (X : in out New_Fixed; S : String) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P3 (A" & S & ")");
      end P3;

      procedure P4 (X : in out Super_Char; S : String) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P4 (A" & S & ")");
      end P4;

      procedure P5 (X : in out A_Color; S : String) is
      begin
         Failed ("EXCEPTION NOT RAISED BEFORE CALL -P5 (A" & S & ")");
      end P5;
   begin
      begin
         P1 (Subint (A0), "1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (A1)");
      end;

      begin
         P1 (Subint (A1), "2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (A2)");
      end;

      begin
         P2 (New_Float (A2), "1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P2 (A1)");
      end;

      begin
         P2 (New_Float (A3), "2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P2 (A2)");
      end;

      begin
         P3 (New_Fixed (A4), "1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P3 (A1)");
      end;

      begin
         P3 (New_Fixed (A5), "2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P3 (A2)");
      end;

      begin
         P4 (Super_Char (A6), "1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P4 (A1)");
      end;

      begin
         P5 (A_Color (A7), "1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P5 (A1)");
      end;
   end;

   declare
      Called : Boolean;
      type Subint is range -8 .. -2;
      A0 : Subint  := -3;
      A1 : Integer := -9;
      A2 : Integer := -1;

      type Float is digits 3 range -1.0 .. 2.0;
      type A_Float is digits 3 range 0.0 .. 1.0;
      A3 : A_Float := 1.0;
      A4 : Float   := -0.5;
      A5 : Float   := 1.5;

      type New_Fixed is delta 1.0 range -1.0 .. 3.0;
      A6 : New_Fixed := 0.0;
      type Fixed_Type is delta 1.0 range -2.0 .. 5.0;
      A7 : Fixed_Type := -2.0;
      A8 : Fixed_Type := 4.0;

      subtype Super_Char is Character range 'B' .. 'Q';
      A9  : Super_Char := 'C';
      A10 : Character  := 'A';
      A11 : Character  := 'R';

      procedure P1 (X : in out Integer; Y : Integer) is
      begin
         Called := True;
         X      := Ident_Int (Y);
      end P1;

      procedure P2 (X : in out Float; Y : Float) is
      begin
         Called := True;
         X      := Y;
      end P2;

      procedure P3 (X : in out Fixed_Type; Y : Fixed_Type) is
      begin
         Called := True;
         X      := Y;
      end P3;

      procedure P4 (X : in out Character; Y : Character) is
      begin
         Called := True;
         X      := Ident_Char (Y);
      end P4;
   begin
      begin
         Called := False;
         P1 (Integer (A0), A1);
         if A0 = -3 then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P1 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P1 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P1 (B1)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B1)");
      end;

      begin
         Called := False;
         P1 (Integer (A0), A2);
         if A0 = -3 then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P1 (B3)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P1 (B4)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P1 (B2)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B2)");
      end;

      begin
         Called := False;
         P2 (Float (A3), A4);
         if A3 = 1.0 then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P2 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P2 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P2 (B1)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P2 (B1)");
      end;

      begin
         Called := False;
         P2 (Float (A3), A5);
         if A3 = 1.0 then
            Failed ("EXCEPTION NOT RAISED -P2 (B3)");
         else
            Failed ("EXCEPTION NOT RAISED -P2 (B4)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P2 (B2)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P2 (B2)");
      end;

      begin
         Called := False;
         P3 (Fixed_Type (A6), A7);
         if A6 = 0.0 then
            Failed ("EXCEPTION NOT RAISED -P3 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED -P3 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P3 (B1)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P3 (B1)");
      end;

      begin
         Called := False;
         P3 (Fixed_Type (A6), A8);
         if A6 = 0.0 then
            Failed ("EXCEPTION NOT RAISED -P3 (B3)");
         else
            Failed ("EXCEPTION NOT RAISED -P3 (B4)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P3 (B2)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P3 (B2)");
      end;

      begin
         Called := False;
         P4 (Character (A9), A10);
         if A9 = 'C' then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P4 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P4 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P4 (B1)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P4 (B1)");
      end;

      begin
         Called := False;
         P4 (Character (A9), A11);
         if A9 = 'C' then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P4 (B3)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P4 (B4)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL " & "-P4 (B2)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P4 (B2)");
      end;
   end;

   Result;
end C64103b;

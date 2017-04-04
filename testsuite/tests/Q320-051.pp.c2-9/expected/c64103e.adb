-- C64103E.ADA

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
--     CHECK THAT, FOR IN-OUT PARAMETERS OF AN ACCESS TYPE,
--     CONSTRAINT_ERROR IS RAISED:
--          BEFORE A SUBPROGRAM CALL WHEN THE BOUNDS OR DISCRIMINANTS
--          OF THE ACTUAL DESIGNATED PARAMETER ARE DIFFERENT FROM
--          THOSE OF THE FORMAL DESIGNATED PARAMETER;
--          AFTER A SUBPROGRAM CALL WHEN THE BOUNDS OR DISCRIMINANTS
--          OF THE FORMAL DESIGNATED PARAMETER ARE DIFFERENT FROM
--          THOSE OF THE ACTUAL DESIGNATED PARAMETER.

-- HISTORY:
--     CPP  07/23/84  CREATED ORIGINAL TEST.
--     VCL  10/27/87  MODIFIED THIS HEADER; ADDED STATEMENTS WHICH
--                    REFERENCED THE ACTUAL PARAMETERS IN THE SECOND
--                    SUBTEST.

with Report; use Report;
procedure C64103e is
begin
   Test
     ("C64103E",
      "FOR IN-OUT PARAMETERS OF AN ACCESS TYPE, " &
      "CONSTRAINT_ERROR IS RAISED:  BEFORE A " &
      "SUBPROGRAM CALL WHEN THE BOUNDS OR " &
      "DISCRIMINANTS OF THE ACTUAL DESIGNATED " &
      "PARAMETER ARE DIFFERENT FROM THOSE OF THE " &
      "FORMAL DESIGNATED PARAMETER;  AFTER A " &
      "SUBPROGRAM CALL WHEN THE BOUNDS OR " &
      "DISCRIMINANTS OF THE FORMAL DESIGNATED " &
      "PARAMETER ARE DIFFERENT FROM THOSE OF THE " &
      "ACTUAL DESIGNATED PARAMETER");

   begin
      declare
         type Ast is access String;
         subtype Ast_3 is Ast (1 .. 3);
         subtype Ast_5 is Ast (3 .. 5);
         X_3 : Ast_3 := new String (1 .. Ident_Int (3));

         procedure P1 (X : in out Ast_5) is
         begin
            Failed ("EXCEPTION NOT RAISED BEFORE CALL -P1 (A)");
         end P1;
      begin
         P1 (Ast_5 (X_3));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (A)");
      end;

      declare
         type Array_Type is array (Integer range <>) of Boolean;
         type A_Array is access Array_Type;
         subtype A1_Array is A_Array (1 .. Ident_Int (3));
         type A2_Array is new A_Array (2 .. 4);
         A0 : A1_Array := new Array_Type (1 .. 3);

         procedure P2 (X : in out A2_Array) is
         begin
            Failed ("EXCEPTION NOT RAISED BEFORE CALL -P2 (A)");
         end P2;
      begin
         P2 (A2_Array (A0));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P2 (A)");
      end;

      declare
         type Subint is range 0 .. 8;
         type Rec1 (Disc : Subint := 8) is record
            Field : Subint := Disc;
         end record;
         type A1_Rec is access Rec1;
         type A2_Rec is new A1_Rec (3);
         A0 : A1_Rec := new Rec1 (4);

         procedure P3 (X : in out A2_Rec) is
         begin
            Failed ("EXCEPTION NOT RAISED BEFORE CALL " & "-P3 (A)");
         end P3;

      begin
         P3 (A2_Rec (A0));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P3 (A)");
      end;

   end;

   begin
      declare
         type Ast is access String;
         subtype Ast_3 is Ast (Ident_Int (1) .. Ident_Int (3));
         X_3    : Ast_3   := new String'(1 .. Ident_Int (3) => 'A');
         Called : Boolean := False;

         procedure P1 (X : in out Ast) is
         begin
            Called := True;
            X      := new String'(3 .. 5 => 'C');
         end P1;
      begin
         P1 (Ast (X_3));
         if X_3.all = String'(1 .. 3 => 'A') then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P1 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P1 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL" & "-P1 (B)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P1 (B)");
      end;

      declare
         type Array_Type is array (Integer range <>) of Boolean;
         type A_Array is access Array_Type;
         subtype A1_Array is A_Array (1 .. Ident_Int (3));
         A0     : A1_Array := new Array_Type'(1 .. 3 => True);
         Called : Boolean  := False;

         procedure P2 (X : in out A_Array) is
         begin
            Called := True;
            X      := new Array_Type'(2 .. 4 => False);
         end P2;
      begin
         P2 (A_Array (A0));
         if A0.all = Array_Type'(1 .. 3 => True) then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P2 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P2 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL" & "-P1 (B)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P2 (B)");
      end;

      declare
         type Subint is range 0 .. 8;
         type Rec1 (Disc : Subint := 8) is record
            Field : Subint := Disc;
         end record;
         type A1_Rec is access Rec1;
         type A2_Rec is new A1_Rec;
         A0     : A1_Rec (4) := new Rec1 (4);
         Called : Boolean    := False;

         procedure P3 (X : in out A2_Rec) is
         begin
            Called := True;
            X      := new Rec1;
         end P3;

      begin
         P3 (A2_Rec (A0));
         if A0.all = Rec1'(4, 4) then
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P3 (B1)");
         else
            Failed ("EXCEPTION NOT RAISED AFTER CALL -P3 (B2)");
         end if;
      exception
         when Constraint_Error =>
            if not Called then
               Failed ("EXCEPTION RAISED BEFORE CALL" & "-P1 (B)");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED -P3 (B)");
      end;

   end;

   Result;
end C64103e;

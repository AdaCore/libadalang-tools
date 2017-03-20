-- C64104A.ADA

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
--    CHECK THAT CONSTRAINT_ERROR IS RAISED FOR OUT OF RANGE SCALAR
--      ARGUMENTS.  SUBTESTS ARE:
--           (A) STATIC IN ARGUMENT.
--           (B) DYNAMIC IN ARGUMENT.
--           (C) IN OUT, OUT OF RANGE ON CALL.
--           (D) OUT, OUT OF RANGE ON RETURN.
--           (E) IN OUT, OUT OF RANGE ON RETURN.

-- HISTORY:
--    DAS  01/14/81
--    CPP  07/03/84
--    LB   11/20/86  ADDED CODE TO ENSURE IN SUBTESTS WHICH CHECK
--                     RETURNED VALUES, THAT SUBPROGRAMS ARE ACTUALLY
--                     CALLED.
--    JET  08/04/87  FIXED HEADER FOR STANDARD FORMAT.

with Report; use Report;
procedure C64104a is

   subtype Digit is Integer range 0 .. 9;

   Called : Boolean;
   D      : Digit;
   I      : Integer;
   M1     : constant Integer := Ident_Int (-1);
   Count  : Integer          := 0;
   subtype Si is Integer range M1 .. 10;

   procedure P1 (Pin : in Digit; Who : String) is         -- (A), (B)
   begin
      Failed ("EXCEPTION NOT RAISED BEFORE CALL - P1 " & Who);
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN P1 FOR " & Who);
   end P1;

   procedure P2 (Pinout : in out Digit; Who : String) is  -- (C)
   begin
      Failed ("EXCEPTION NOT RAISED BEFORE CALL - P2 " & Who);
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN P2 FOR " & Who);
   end P2;

   procedure P3 (Pout : out Si; Who : String) is          -- (D)
   begin
      if Who = "10" then
         Pout := Ident_Int (10);    -- (10 IS NOT A DIGIT)
      else
         Pout := -1;
      end if;
      Called := True;
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN P3 FOR " & Who);
   end P3;

   procedure P4 (Pinout : in out Integer; Who : String) is     -- (E)
   begin
      if Who = "10" then
         Pinout := 10;       -- (10 IS NOT A DIGIT)
      else
         Pinout := Ident_Int (-1);
      end if;
      Called := True;
   exception
      when others =>
         Failed ("EXCEPTION RAISED IN P4 FOR" & Who);
   end P4;

begin

   Test
     ("C64104A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "FOR OUT OF RANGE SCALAR ARGUMENTS");

   begin  -- (A)
      P1 (10, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR P1 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P1 (10)");
   end;  -- (A)

   begin  -- (B)
      P1 (Ident_Int (-1), "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR P1 (" & "IDENT_INT (-1))");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P1 (" & "IDENT_INT (-1))");
   end;  --(B)

   begin  -- (C)
      I := Ident_Int (10);
      P2 (I, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR P2 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P2 (10)");
   end;  -- (C)

   begin -- (C1)
      I := Ident_Int (-1);
      P2 (I, "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR P2 (-1)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P2 (-1)");
   end; -- (C1)

   begin  -- (D)
      Called := False;
      D      := Ident_Int (1);
      P3 (D, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" & " P3 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("SUBPROGRAM P3 WAS NOT CALLED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P3 (10)");
   end;  -- (D)

   begin -- (D1)
      Called := False;
      D      := Ident_Int (1);
      P3 (D, "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" & " P3 (-1)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("SUBPROGRAM P3 WAS NOT CALLED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P3 (-1)");
   end; -- (D1)

   begin  -- (E)
      Called := False;
      D      := 9;
      P4 (D, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" & " P4 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("SUBPROGRAM P4 WAS NOT CALLED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P4 (10)");
   end;  -- (E)

   begin -- (E1)
      Called := False;
      D      := 0;
      P4 (D, "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" & " P4 (-1)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("SUBPROGRAM P4 WAS NOT CALLED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR P4 (-1)");
   end; -- (E1)

   if (Count /= 8) then
      Failed ("INCORRECT NUMBER OF CONSTRAINT_ERRORS RAISED");
   end if;

   Result;

end C64104a;

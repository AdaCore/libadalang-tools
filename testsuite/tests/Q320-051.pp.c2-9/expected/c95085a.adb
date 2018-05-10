-- C95085A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR OUT OF RANGE SCALAR ARGUMENTS.
-- SUBTESTS ARE:
--        (A) STATIC IN ARGUMENT.
--        (B) DYNAMIC IN ARGUMENT.
--        (C) IN OUT, OUT OF RANGE ON CALL.
--        (D) OUT, OUT OF RANGE ON RETURN.
--        (E) IN OUT, OUT OF RANGE ON RETURN.

-- GLH 7/15/85
-- JRK 8/23/85
-- JWC 11/15/85 ADDED VARIABLE "CALLED" TO ENSURE THAT THE ENTRY
--                  CALL WAS MADE FOR THOSE CASES THAT ARE APPLICABLE.

with Report; use Report;
procedure C95085a is

   subtype Digit is Integer range 0 .. 9;

   D      : Digit;
   I      : Integer;
   M1     : constant Integer := Ident_Int (-1);
   Count  : Integer          := 0;
   Called : Boolean;

   subtype Si is Integer range M1 .. 10;

   task T1 is
      entry E1 (Pin : in Digit; Who : String);  -- (A), (B).
   end T1;

   task body T1 is
   begin
      loop
         begin
            select
               accept E1 (Pin : in Digit; Who : String) do  -- (A), (B).
                  Failed ("EXCEPTION NOT RAISED BEFORE " & "CALL - E1 " & Who);
               end E1;
            or
               terminate;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN E1");
         end;
      end loop;
   end T1;

   task T2 is
      entry E2 (Pinout : in out Digit; Who : String);  -- (C).
   end T2;

   task body T2 is
   begin
      loop
         begin
            select
               accept E2 (Pinout : in out Digit; Who : String) do  -- (C).
                  Failed ("EXCEPTION NOT RAISED BEFORE " & "CALL - E2 " & Who);
               end E2;
            or
               terminate;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN E2");
         end;
      end loop;
   end T2;

   task T3 is
      entry E3 (Pout : out Si; Who : String);  -- (D).
   end T3;

   task body T3 is
   begin
      loop
         begin
            select
               accept E3 (Pout : out Si; Who : String) do  -- (D).
                  Called := True;
                  if Who = "10" then
                     Pout := Ident_Int (10);  -- 10 IS NOT
                     -- A DIGIT.
                  else
                     Pout := -1;
                  end if;
               end E3;
            or
               terminate;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN E3");
         end;
      end loop;
   end T3;

   task T4 is
      entry E4 (Pinout : in out Integer; Who : String);  -- (E).
   end T4;

   task body T4 is
   begin
      loop
         begin
            select
               accept E4 (Pinout : in out Integer; Who : String) do  -- (E).
                  Called := True;
                  if Who = "10" then
                     Pinout := 10;  -- 10 IS NOT A DIGIT.
                  else
                     Pinout := Ident_Int (-1);
                  end if;
               end E4;
            or
               terminate;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED IN E4");
         end;
      end loop;
   end T4;

begin

   Test
     ("C95085A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "FOR OUT OF RANGE SCALAR ARGUMENTS");

   begin  -- (A)
      T1.E1 (10, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR E1 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E1 (10)");
   end;  -- (A)

   begin  -- (B)
      T1.E1 (Ident_Int (-1), "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR E1 (" & "IDENT_INT (-1))");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E1 (" & "IDENT_INT (-1))");
   end;  -- (B)

   begin  -- (C)
      I := Ident_Int (10);
      T2.E2 (I, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR E2 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E2 (10)");
   end;  -- (C)

   begin -- (C1)
      I := Ident_Int (-1);
      T2.E2 (I, "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR E2 (-1)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E2 (-1)");
   end; -- (C1)

   begin  -- (D)
      Called := False;
      D      := Ident_Int (1);
      T3.E3 (D, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " & "E3 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL " & "E3 (10)");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E3 (10)");
   end;  -- (D)

   begin -- (D1)
      Called := False;
      D      := Ident_Int (1);
      T3.E3 (D, "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " & "E3 (-1)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL " & "E3 (-1)");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E3 (-1)");
   end; -- (D1)

   begin  -- (E)
      Called := False;
      D      := 9;
      T4.E4 (D, "10");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " & "E4 (10)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL " & "E4 (10)");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E4 (10)");
   end;  -- (E)

   begin -- (E1)
      Called := False;
      D      := 0;
      T4.E4 (D, "-1");
      Failed ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " & "E4 (-1)");
   exception
      when Constraint_Error =>
         Count := Count + 1;
         if not Called then
            Failed ("EXCEPTION RAISED BEFORE CALL " & "E4 (-1)");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR E4 (-1)");
   end; -- (E1)

   if Count /= 8 then
      Failed ("INCORRECT NUMBER OF CONSTRAINT_ERRORS RAISED");
   end if;

   Result;

end C95085a;

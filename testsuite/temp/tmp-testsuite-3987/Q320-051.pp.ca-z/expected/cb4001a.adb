-- CB4001A.ADA

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
-- CHECK THAT ANY EXCEPTION RAISED IN THE STATEMENT SEQUENCE OF A
-- SUBPROGRAM IS PROPAGATED TO THE CALLER OF THE SUBPROGRAM, NOT TO THE
-- STATICALLY ENCLOSING LEXICAL ENVIRONMENT.

-- RM  05/30/80
-- JRK 11/19/80
-- SPS 03/28/83
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

with Report;
procedure Cb4001a is

   use Report;

   E1 : exception;
   I9         : Integer range 1 .. 10;
   Flow_Count : Integer := 0;

begin
   Test
     ("CB4001A",
      "CHECK THAT ANY EXCEPTION RAISED IN THE " &
      "STATEMENT SEQUENCE OF A SUBPROGRAM IS " &
      "PROPAGATED TO THE CALLER, NOT TO THE STATICALLY ENCLOSING" &
      " LEXICAL ENVIRONMENT");

   begin   --  BLOCK WITH HANDLERS; LEX. ENVIRONMT FOR ALL PROC.DEFS

      declare   --  BLOCK WITH PROCEDURE DEFINITIONS

         procedure Callee1;
         procedure Callee2;
         procedure Callee3;
         procedure R;
         procedure S;

         procedure Caller1 is
         begin
            Flow_Count := Flow_Count + 1;
            Callee1;
            Failed ("EXCEPTION NOT RAISED  (CALLER1)");
         exception
            when E1 =>
               Flow_Count := Flow_Count + 1;
         end Caller1;

         procedure Caller2 is
         begin
            Flow_Count := Flow_Count + 1;
            Callee2;
            Failed ("EXCEPTION NOT RAISED  (CALLER2)");
         exception
            when Constraint_Error =>
               Flow_Count := Flow_Count + 1;
         end Caller2;

         procedure Caller3 is
         begin
            Flow_Count := Flow_Count + 1;
            Callee3;
            Failed ("EXCEPTION NOT RAISED  (CALLER3)");
         exception
            when Constraint_Error =>
               Flow_Count := Flow_Count + 1;
         end Caller3;

         procedure Callee1 is
         begin
            Flow_Count := Flow_Count + 1;
            R;
            Failed ("EXCEPTION NOT RAISED  (CALLEE1)");
         end Callee1;

         procedure Callee2 is
         begin
            Flow_Count := Flow_Count + 1;
            raise Constraint_Error;
            Failed ("EXCEPTION NOT RAISED  (CALLEE2)");
         exception
            when Program_Error =>
               Failed ("WRONG EXCEPTION RAISED  (CALLEE2)");
         end Callee2;

         procedure Callee3 is
         begin
            Flow_Count := Flow_Count + 1;
            I9         := Ident_Int (20);
            Failed ("EXCEPTION NOT RAISED  (CALLEE3)");
         end Callee3;

         procedure R is
            E2 : exception;
         begin
            Flow_Count := Flow_Count + 10;
            S;
            Failed ("EXCEPTION E1 NOT RAISED (PROC R)");
         exception
            when E2 =>
               Failed ("WRONG EXCEPTION RAISED  (PROC R)");
         end R;

         procedure S is
         begin
            Flow_Count := Flow_Count + 10;
            raise E1;
            Failed ("EXCEPTION  E1  NOT RAISED  (PROC S)");
         end S;

      begin   --  (THE BLOCK WITH PROC. DEFS)

         Caller1;
         Caller2;
         Caller3;

      end;   --  (THE BLOCK WITH PROC. DEFS)

   exception

      when others =>
         Failed ("EXCEPTION PROPAGATED STATICALLY");

   end;

   if Flow_Count /= 29 then
      Failed ("INCORRECT FLOW_COUNT VALUE");
   end if;

   Result;
end Cb4001a;

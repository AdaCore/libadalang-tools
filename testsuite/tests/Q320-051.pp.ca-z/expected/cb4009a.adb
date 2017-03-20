-- CB4009A.ADA

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
-- CHECK THAT A PROGRAMMER DEFINED EXCEPTION AND A REDECLARED
-- PREDEFINED EXCEPTION MAY BE PROPAGATED OUT OF SCOPE AND BACK IN,
-- WITH OUT-OF-SCOPE 'OTHERS' HANDLERS HANDLING THE EXCEPTION
-- INSTEAD OF OTHER HANDLERS. SEPARATELY COMPILED UNITS ARE NOT TESTED.

-- DAT 4/15/81
-- SPS 1/14/82

with Report; use Report;

procedure Cb4009a is

   E : exception;

   I : Integer := 0;

   procedure P1 (C : Integer);
   procedure P2 (C : Integer);
   procedure P3 (C : Integer);

   F : Boolean          := False;
   T : constant Boolean := True;

   procedure P1 (C : Integer) is
   begin
      P3 (C);
   exception
      when E =>
         F := T;
      when Constraint_Error =>
         F := T;
      when others =>
         I := I + 1;
         raise;
   end P1;

   procedure P2 (C : Integer) is
      E                : exception;
      Constraint_Error : exception;
   begin
      case C is
         when 0 =>
            Failed ("WRONG CASE");
         when 1 =>
            raise E;
         when -1 =>
            raise Constraint_Error;
         when others =>
            P1 (C - C / abs (C));
      end case;
   exception
      when E =>
         I := I + 100;
         raise;
      when Constraint_Error =>
         I := I + 101;
         raise;
      when others =>
         F := T;
   end P2;

   procedure P3 (C : Integer) is
   begin
      P2 (C);
   exception
      when E =>
         F := T;
      when Constraint_Error =>
         F := T;
   end P3;

begin
   Test ("CB4009A", "EXCEPTIONS PROPAGATED OUT OF SCOPE");

   I := 0;
   begin
      P3 (-2);
      Failed ("EXCEPTION NOT RAISED 1");
   exception
      when others =>
         null;
   end;
   if I /= 203 then
      Failed ("INCORRECT HANDLER SOMEWHERE 1");
   end if;

   I := 0;
   begin
      P3 (3);
      Failed ("EXCEPTION NOT RAISED 2");
   exception
      when others =>
         null;
   end;
   if I /= 302 then
      Failed ("INCORRECT HANDLER SOMEWHERE 2");
   end if;

   if F = T then
      Failed ("WRONG HANDLER SOMEWHERE");
   end if;

   Result;
end Cb4009a;

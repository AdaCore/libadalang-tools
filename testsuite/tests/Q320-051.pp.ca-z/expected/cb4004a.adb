-- CB4004A.ADA

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
-- CHECK THAT VARIOUS EXCEPTIONS IN THE BODY OF A SUBPROGRAM WITH
-- AN APPLICABLE HANDLER ARE HANDLED LOCALLY.

-- DAT 04/15/81
-- JRK 04/24/81
-- SPS 11/02/82
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

with Report; use Report;

procedure Cb4004a is

   E, F          : exception;
   Storage_Error : exception;

   I1 : Integer range 1 .. 1;

   function F1 (I : Integer) return Boolean is
   begin
      case I is
         when 1 =>
            raise E;
         when 2 =>
            raise Storage_Error;
         when 3 =>
            I1 := 4;
         when 4 =>
            raise Tasking_Error;
         when others =>
            null;
      end case;
      return False;
   exception
      when E | F =>
         return I = 1;
      when Storage_Error =>
         return I = 2;
      when Program_Error | Constraint_Error =>
         return I = 3;
      when others =>
         return I = 4;
   end F1;

begin
   Test ("CB4004A", "EXCEPTIONS WITH LOCAL HANDLERS ARE HANDLED" & " THERE");

   begin
      for L in 1 .. 4 loop
         if F1 (L) /= True then
            Failed ("LOCAL EXCEPTIONS DON'T WORK");
            exit;
         end if;
      end loop;
   exception
      when others =>
         Failed ("WRONG HANDLER");
   end;

   Result;
end Cb4004a;

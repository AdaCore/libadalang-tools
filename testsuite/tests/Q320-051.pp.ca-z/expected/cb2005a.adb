-- CB2005A.ADA

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
-- CHECK THAT A RETURN STATEMENT CAN APPEAR IN AN EXCEPTION HANDLER AND IT
-- CAUSES CONTROL TO LEAVE THE SUBPROGRAM, FOR BOTH FUNCTIONS AND PROCEDURES.

-- DAT 4/13/81
-- JRK 4/24/81
-- SPS 10/26/82

with Report; use Report;

procedure Cb2005a is

   I : Integer range 0 .. 1;

   function Seti return Integer is
   begin
      I := I + 1;
      Failed ("CONSTRAINT_ERROR NOT RAISED 1");
      return 0;
   exception
      when others =>
         return I;
         Failed ("FUNCTION RETURN STMT DID NOT RETURN");
         return 0;
   end Seti;

   procedure Iset is
   begin
      I := 2;
      Failed ("CONSTRAINT_ERROR NOT RAISED 2");
      I := 0;
   exception
      when others =>
         return;
         Failed ("PROCEDURE RETURN STMT DID NOT RETURN");
   end Iset;

begin
   Test ("CB2005A", "RETURN IN EXCEPTION HANDLERS");

   I := 1;
   if Seti /= 1 then
      Failed ("WRONG VALUE RETURNED 1");
   end if;

   I := 1;
   Iset;
   if I /= 1 then
      Failed ("WRONG VALUE RETURNED 2");
   end if;

   Result;
end Cb2005a;

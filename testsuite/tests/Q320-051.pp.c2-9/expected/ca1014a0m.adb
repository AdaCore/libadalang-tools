-- CA1014A0M.ADA

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
-- CHECK THAT A SUBUNIT CAN BE SUBMITTED FOR COMPILATION SEPARATELY FROM ITS
-- PARENT UNIT.

-- SEPARATE FILES ARE:
--   CA1014A0M THE MAIN PROCEDURE.
--   CA1014A1  A SUBUNIT PROCEDURE BODY.
--   CA1014A2  A SUBUNIT PACKAGE BODY.
--   CA1014A3  A SUBUNIT FUNCTION BODY.

-- JRK 5/20/81

with Report; use Report;

procedure Ca1014a0m is

   I : Integer := 0;

   package Call_Test is
   end Call_Test;

   package body Call_Test is
   begin
      Test
        ("CA1014A",
         "SUBUNITS SUBMITTED FOR COMPILATION " &
         "SEPARATELY FROM PARENT UNIT");
   end Call_Test;

   procedure Ca1014a1 (I : in out Integer) is separate;

   package Ca1014a2 is
      I : Integer := 10;
      procedure P (I : in out Integer);
   end Ca1014a2;

   package body Ca1014a2 is separate;

   function Ca1014a3 (I : Integer) return Integer is separate;

begin

   Ca1014a1 (I);
   if I /= 1 then
      Failed ("SUBUNIT PROCEDURE NOT ELABORATED/EXECUTED");
   end if;

   if Ca1014a2.I /= 15 then
      Failed ("SUBUNIT PACKAGE BODY NOT ELABORATED/EXECUTED");
   end if;

   I := 0;
   Ca1014a2.P (I);
   if I /= -20 then
      Failed ("SUBUNIT PACKAGED PROCEDURE NOT ELABORATED/EXECUTED");
   end if;

   if Ca1014a3 (50) /= -50 then
      Failed ("SUBUNIT FUNCTION NOT ELABORATED/EXECUTED");
   end if;

   Result;
end Ca1014a0m;

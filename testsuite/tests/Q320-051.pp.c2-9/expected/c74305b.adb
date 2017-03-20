-- C74305B.ADA

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
--    CHECK THAT A DEFERRED CONSTANT CAN BE USED AS A DEFAULT
--    INITIALIZATION FOR A PARAMETER OR AS A DEFAULT INITIA-
--    LIZATION FOR A COMPONENT (GENERIC CASE).

-- EG  12/20/83

with Report;

procedure C74305b is

   use Report;

   package Pk is
      type Td is private;
      Cd : constant Td;
      Dd : constant Td;

      generic
         type T1 is private;
         C1 : T1;
         with procedure P2 (A1 : T1 := C1; A2 : Td := Cd);
      procedure P1 (A1 : Td := Cd);

   private
      type Td is new Integer;
      Cd : constant Td := 2;
      Dd : constant Td := 3;
   end Pk;

   use Pk;

   package body Pk is

      procedure P1 (A1 : Td := Cd) is
      begin
         if (A1 /= 2) then
            Failed ("WRONG ACTUAL PARAMETER RECEIVED (1)");
         end if;
         P2;
      end P1;

      procedure P3 (X : Td := Dd; Y : Td := Dd) is
      begin
         if (X /= 2) then
            Failed ("WRONG ACTUAL PARAMETER RECEIVED (2)");
         end if;
         if (Y /= 2) then
            Failed ("WRONG ACTUAL PARAMETER RECEIVED (3)");
         end if;
      end P3;

      procedure P4 is new P1 (Td, Cd, P3);

   begin
      Test
        ("C74305B",
         "CHECK THAT A DEFERRED CONSTANT CAN BE " &
         "USED AS A DEFAULT INITIALIZATION FOR A " &
         "PARAMETER OR AS A DEFAULT INITIALIZATION " &
         "FOR A COMPONENT (GENERIC CASE)");
      P4;
   end Pk;

   procedure P5 (X : Td := Dd; Y : Td := Dd) is
   begin
      if (X /= Cd) then
         Failed ("WRONG ACTUAL PARAMETER RECEIVED (4)");
      end if;
      if (Y /= Cd) then
         Failed ("WRONG ACTUAL PARAMETER RECEIVED (5)");
      end if;
   end P5;

   procedure P6 is new P1 (Td, Cd, P5);

begin
   P6;
   Result;
end C74305b;

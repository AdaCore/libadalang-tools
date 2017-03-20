-- C95009A.ADA

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
-- CHECK THAT A TASK OBJECT CAN CALL ENTRIES OF OTHER TASKS.

-- THIS TEST CONTAINS SHARED VARIABLES.

-- JRK 11/5/81
-- JRK 8/3/84

with Report; use Report;
procedure C95009a is

   V1 : Integer := 0;
   V2 : Integer := 0;

   Pi : Integer := 0;
   Po : Integer := 0;

begin
   Test
     ("C95009A",
      "CHECK THAT A TASK OBJECT CAN CALL ENTRIES " & "OF OTHER TASKS");

   declare

      subtype Int is Integer range 1 .. 5;

      task T1 is
         entry E1n;
         entry Ef1p (Int) (I : out Integer);
      end T1;

      task type T2t is
         entry E2p (I : Integer);
         entry Ef2n (Int);
      end T2t;

      type At2t is access T2t;
      At2 : At2t;

      task body T1 is
      begin
         V1 := 1;
         accept E1n;
         V1 := 2;
         At2.E2p (1);
         V1 := 3;
         accept Ef1p (2) (I : out Integer) do
            I := 2;
         end Ef1p;
         V1 := 4;
         At2.Ef2n (Ident_Int (3));
         V1 := 5;
      end T1;

      task body T2t is
      begin
         V2 := 1;
         T1.E1n;
         V2 := 2;
         accept E2p (I : Integer) do
            Pi := I;
         end E2p;
         V2 := 3;
         T1.Ef1p (2) (Po);
         V2 := 4;
         accept Ef2n (1 + Ident_Int (2));
         V2 := 5;
      end T2t;

      package Dummy is
      end Dummy;

      package body Dummy is
      begin
         At2 := new T2t;
      end Dummy;

   begin
      null;
   end;

   if V1 /= 5 then
      Failed ("TASK T1 ONLY REACHED V1 = " & Integer'Image (V1));
   end if;

   if V2 /= 5 then
      Failed ("TASK AT2 ONLY REACHED V2 = " & Integer'Image (V2));
   end if;

   if Pi /= 1 then
      Failed ("ENTRY IN PARAMETER NOT PASSED CORRECTLY");
   end if;

   if Po /= 2 then
      Failed ("ENTRY OUT PARAMETER NOT PASSED CORRECTLY");
   end if;

   Result;
end C95009a;

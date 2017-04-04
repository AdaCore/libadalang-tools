-- C95012A.ADA

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
-- CHECK THAT A CALL TO AN ENTRY OF A TASK THAT HAS NOT BEEN ACTIVATED
--   DOES NOT RAISE EXCEPTIONS.

-- THIS TEST CONTAINS RACE CONDITIONS.

-- JRK 11/6/81
-- SPS 11/21/82
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C95012a is

   I : Integer := 0;

begin
   Test
     ("C95012A",
      "CHECK THAT A CALL TO AN ENTRY OF A TASK " &
      "THAT HAS NOT BEEN ACTIVATED DOES NOT " &
      "RAISE EXCEPTIONS");

   declare

      task T1 is
         entry E1 (I : out Integer);
      end T1;

      task type T2t is
         entry E2 (I : out Integer);
      end T2t;

      type At2t is access T2t;
      At2 : At2t;

      task body T1 is
      begin
         accept E1 (I : out Integer) do
            I := Ident_Int (1);
         end E1;
      end T1;

      task body T2t is
         J : Integer := 0;
      begin
         begin
            T1.E1 (J);
         exception
            when others =>
               J := -1;
         end;
         accept E2 (I : out Integer) do
            I := J;
         end E2;
      end T2t;

      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         At2 := new T2t;
         delay 60.0;
      end Pkg;

   begin

      At2.all.E2 (I);

      if I = -1 then
         Failed ("EXCEPTION RAISED");
         T1.E1 (I);
      end if;

      if I /= 1 then
         Failed ("WRONG VALUE PASSED");
      end if;

   end;

   Result;
end C95012a;

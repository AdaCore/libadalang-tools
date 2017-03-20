-- C55B05A.ADA

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
-- CHECK THAT LOOPS WITH BOUNDS INTEGER'LAST OR
-- INTEGER'FIRST DO NOT RAISE INVALID EXCEPTIONS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- DAT 3/26/81
-- SPS 3/2/83
-- MRM 03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report; use Report;

procedure C55b05a is
begin
   Test ("C55B05A", "LOOPS WITH INTEGER'FIRST AND 'LAST AS BOUNDS");

   declare

      Count : Integer := 0;

      procedure C is
      begin
         Count := Count + 1;
      end C;

   begin
      for I in Integer'Last .. Integer'First loop
         Failed ("WRONG NULL RANGE LOOP EXECUTION");
         exit;
      end loop;
      for I in Integer'First .. Integer'First loop
         C;
      end loop;
      for I in Integer'First .. Integer'First + 2 loop
         C;
         C;
      end loop;
      for I in Integer'First + 1 .. Integer'First loop
         Failed ("NULL RANGE ERROR 2");
         exit;
      end loop;
      for I in Integer'First .. Integer'Last loop
         C;
         exit;
      end loop;
      for I in Integer loop
         C;
         exit;
      end loop;
      for I in Integer'Last - 2 .. Integer'Last loop
         C;
         C;
         C;
      end loop;
      for I in Integer'Last - 2 .. Integer'Last - 1 loop
         C;
      end loop;
      for I in 0 .. Integer'First loop
         Failed ("NULL LOOP ERROR 3");
         exit;
      end loop;
      for I in -1 .. Integer'First loop
         Failed ("NULL LOOP ERROR 4");
         exit;
      end loop;
      for I in -3 .. Ident_Int (0) loop
         for J in Integer'First .. Integer'First - I loop
            C;
            C;
            C;
            C;
         end loop;
         for J in Integer'First - I .. Integer'First + 3 - I loop
            C;
            C;
            C;
            C;
         end loop;
         for J in Integer'Last - 3 .. Integer'Last + I loop
            C;
            C;
            C;
            C;
         end loop;
         for J in Integer'Last + I .. Integer'Last loop
            C;
            C;
            C;
            C;
         end loop;
      end loop;

      for I in reverse Integer'Last .. Integer'First loop
         Failed ("REVERSE WRONG NULL RANGE LOOP EXECUTION");
         exit;
      end loop;
      for I in reverse Integer'First .. Integer'First loop
         C;
      end loop;
      for I in reverse Integer'First .. Integer'First + 2 loop
         C;
         C;
      end loop;
      for I in reverse Integer'First + 1 .. Integer'First loop
         Failed ("NULL RANGE ERROR 8");
         exit;
      end loop;
      for I in reverse Integer'First .. Integer'Last loop
         C;
         exit;
      end loop;
      for I in reverse Integer loop
         C;
         exit;
      end loop;
      for I in reverse Integer'Last - 2 .. Integer'Last loop
         C;
         C;
         C;
      end loop;
      for I in reverse Integer'Last - 2 .. Integer'Last - 1 loop
         C;
      end loop;
      for I in reverse 0 .. Integer'First loop
         Failed ("NULL LOOP ERROR 9");
         exit;
      end loop;
      for I in reverse -1 .. Integer'First loop
         Failed ("NULL LOOP ERROR 7");
         exit;
      end loop;
      for I in reverse -3 .. Ident_Int (0) loop
         for J in reverse Integer'First .. Integer'First - I loop
            C;
            C;
            C;
            C;
         end loop;
         for J in reverse Integer'First - I .. Integer'First + 3 - I loop
            C;
            C;
            C;
            C;
         end loop;
         for J in reverse Integer'Last - 3 .. Integer'Last + I loop
            C;
            C;
            C;
            C;
         end loop;
         for J in reverse Integer'Last + I .. Integer'Last loop
            C;
            C;
            C;
            C;
         end loop;
      end loop;

      if Count /= 408 then
         Failed ("WRONG LOOP EXECUTION COUNT");
      end if;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED INCORRECTLY");
      when others =>
         Failed ("UNKNOWN EXCEPTION RAISED INCORRECTLY");
   end;

   Result;
end C55b05a;

-- C36301A.ADA

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
-- CHECK THAT PREDEFINED POSITIVE AND STRING TYPES
-- ARE CORRECTLY DEFINED.

-- DAT 2/17/81
-- JBG 12/27/82
-- RJW 1/20/86 - CHANGED 'NATURAL' TO 'POSITIVE'.  ADDED ADDITIONAL
--               CASES, INCLUDING A CHECK FOR STRINGS WITH BOUNDS
--               OF INTEGER'FIRST AND INTEGER'LAST.
-- EDS 7/16/98   AVOID OPTIMIZATION

with Report; use Report;

procedure C36301a is

begin
   Test ("C36301A", "CHECK ATTRIBUTES OF PREDEFINED POSITIVE " & "AND STRING");

   begin
      if Positive'First /= 1 then
         Failed ("POSITIVE'FIRST IS WRONG");
      end if;

      if Positive'Last /= Integer'Last then
         Failed ("POSITIVE'LAST IS WRONG");
      end if;
   end;

   declare

      C : String (1 .. 2) := ('A', 'B');

   begin
      if C'Length /= 2 then
         Failed ("LENGTH OF C IS WRONG");
      end if;

      if C'First /= 1 then
         Failed ("C'FIRST IS WRONG");
      end if;

      if C'Last /= 2 then
         Failed ("C'LAST IS WRONG");
      end if;
   end;

   declare

      subtype Large is String (Integer'Last - 3 .. Integer'Last);

   begin
      if Large'Length /= 4 then
         Failed ("LENGTH OF LARGE IS WRONG");
      end if;

      if Large'First /= Integer'Last - 3 then
         Failed ("LARGE'FIRST IS WRONG");
      end if;

      if Large'Last /= Integer'Last then
         Failed ("LARGE'LAST IS WRONG");
      end if;
   end;

   declare

      subtype Larger is String (1 .. Integer'Last);

   begin
      if Larger'Length /= Integer'Last then
         Failed ("LENGTH OF LARGER IS WRONG");
      end if;

      if Larger'First /= 1 then
         Failed ("LARGER'FIRST IS WRONG");
      end if;

      if Larger'Last /= Integer'Last then
         Failed ("LARGER'LAST IS WRONG");
      end if;
   end;

   begin
      declare

         D : String (Integer'First .. Integer'First + 3);

      begin
         if D'First /= Integer'First then   -- USE D
            Failed ("D'FIRST IS INCORRECT " & Integer'Image (D'First));
         end if;
         Failed ("NO EXCEPTION RAISED");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
   end;

   begin
      declare

         E : String (-1 .. Integer'First);

      begin
         if E'Length /= 0 then
            Failed ("LENGTH OF E IS WRONG");
         end if;

         if E'First /= -1 then
            Failed ("E'FIRST IS WRONG");
         end if;

         if E'Last /= Integer'First then
            Failed ("E'LAST IS WRONG");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED FOR NULL STRING");
   end;

   Result;
end C36301a;

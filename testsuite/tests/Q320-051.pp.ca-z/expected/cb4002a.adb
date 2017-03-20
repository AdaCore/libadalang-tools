-- CB4002A.ADA

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
-- CHECK THAT EXCEPTIONS RAISED DURING ELABORATION OF THE
-- DECLARATIVE PART OF A SUBPROGRAM ARE PROPAGATED TO THE
-- CALLER, FOR CONSTRAINT_ERROR CAUSED BY INITIALIZATION,
-- AND CONSTRAINT ELABORATION, AND FOR FUNCTION EVALUATIONS
-- RAISING  CONSTRAINT_ERROR AND A PROGRAMMER-DEFINED EXCEPTION.

-- DAT 4/13/81
-- SPS 3/28/83

with Report; use Report;

procedure Cb4002a is
begin
   Test
     ("CB4002A",
      "EXCEPTIONS IN SUBPROGRAM DECLARATIVE_PARTS" &
      " ARE PROPAGATED TO CALLER");

   declare
      subtype I5 is Integer range -5 .. 5;

      E : exception;

      function Raise_It (I : I5) return Integer is
         J : Integer range 0 .. 1 := I;
      begin
         if I = 0 then
            raise Constraint_Error;
         elsif I = 1 then
            raise E;
         end if;
         Failed ("EXCEPTION NOT RAISED 0");
         return J;
      exception
         when others =>
            if I not in 0 .. 1 then
               Failed ("WRONG HANDLER 0");
               return 0;
            else
               raise;
            end if;
      end Raise_It;

      procedure P1 (P : Integer) is
         Q : Integer := Raise_It (P);
      begin
         Failed ("EXCEPTION NOT RAISED 1");
      exception
         when others =>
            Failed ("WRONG HANDLER 1");
      end P1;

      procedure P2 (P : Integer) is
         Q : I5 range 0 .. P := 1;
      begin
         if P = 0 or P > 5 then
            Failed ("EXCEPTION NOT RAISED 2");
         end if;
      end P2;

   begin

      begin
         P1 (-1);
         Failed ("EXCEPTION NOT RAISED 2A");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         P1 (0);
         Failed ("EXCEPTION NOT RAISED 3");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         P1 (1);
         Failed ("EXCEPTION NOT RAISED 4");
      exception
         when E =>
            null;
      end;

      begin
         P2 (0);
         Failed ("EXCEPTION NOT RAISED 5");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         P2 (6);
         Failed ("EXCEPTION NOT RAISED 6");
      exception
         when Constraint_Error =>
            null;
      end;

   exception
      when others =>
         Failed ("WRONG EXCEPTION OR HANDLER");
   end;

   Result;
exception
   when others =>
      Failed ("WRONG HANDLER FOR SURE");
      Result;
end Cb4002a;

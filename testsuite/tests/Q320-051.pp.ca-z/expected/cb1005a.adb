-- CB1005A.ADA

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
-- CHECK THAT EXCEPTIONS DECLARED IN GENERIC PACKAGES AND PROCEDURES ARE
-- CONSIDERED DISTINCT FOR EACH INSTANTIATION.

-- CHECK THAT AN EXCEPTION NAME DECLARED IN A GENERIC PACKAGE
-- INSTANTIATION IN A RECURSIVE PROCEDURE DENOTES THE SAME ENTITY
-- EVEN WHEN THE INSTANTIATION IS ELABORATED MORE THAN ONCE BECAUSE
-- OF RECURSIVE CALLS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- TBN  9/23/86
-- MRM  03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report; use Report;
procedure Cb1005a is

   procedure Prop;

   generic
   package Pac is
      Exc : exception;
   end Pac;

   generic
   procedure Proc (Inst_Again : Boolean);

   procedure Proc (Inst_Again : Boolean) is
      Exc : exception;
   begin
      if Inst_Again then
         begin
            Prop;
            Failed ("EXCEPTION WAS NOT PROPAGATED - 9");
         exception
            when Exc =>
               Failed ("EXCEPTION NOT DISTINCT - 10");
            when Program_Error | Storage_Error | Tasking_Error | Constraint_Error =>
               Failed ("WRONG EXCEPTION PROPAGATED - 11");
            when others =>
               null;
         end;
      else
         raise Exc;
      end if;
   end Proc;

   procedure Raise_Exc (Call_Again : Boolean) is
      package Pac3 is new Pac;
   begin
      if Call_Again then
         begin
            Raise_Exc (False);
            Failed ("EXCEPTION WAS NOT PROPAGATED - 12");
         exception
            when Pac3.Exc =>
               null;
         end;
      else
         raise Pac3.Exc;
      end if;
   end Raise_Exc;

   procedure Prop is
      procedure Proc2 is new Proc;
   begin
      Proc2 (False);
   end Prop;

begin
   Test
     ("CB1005A",
      "CHECK THAT EXCEPTIONS DECLARED IN GENERIC " &
      "PACKAGES AND PROCEDURES ARE CONSIDERED " &
      "DISTINCT FOR EACH INSTANTIATION");

   -------------------------------------------------------------------
   declare
      package Pac1 is new Pac;
      package Pac2 is new Pac;
      Pac1_Exc_Found : Boolean := False;
   begin
      begin
         if Equal (3, 3) then
            raise Pac2.Exc;
         end if;
         Failed ("EXCEPTION WAS NOT RAISED - 1");

      exception
         when Pac1.Exc =>
            Failed ("PACKAGE EXCEPTIONS NOT DISTINCT - 2");
            Pac1_Exc_Found := True;
      end;
      if not Pac1_Exc_Found then
         Failed ("EXCEPTION WAS NOT PROPAGATED - 3");
      end if;

   exception
      when Pac1.Exc =>
         Failed ("PACKAGE EXCEPTIONS NOT DISTINCT - 4");
      when Pac2.Exc =>
         begin
            if Equal (3, 3) then
               raise Pac1.Exc;
            end if;
            Failed ("EXCEPTION WAS NOT RAISED - 5");

         exception
            when Pac2.Exc =>
               Failed ("PACKAGE EXCEPTIONS NOT DISTINCT - 6");
            when Pac1.Exc =>
               null;
            when others =>
               Failed ("UNKNOWN EXCEPTION RAISED - 7");
         end;
      when others =>
         Failed ("UNKNOWN EXCEPTION RAISED - 8");
   end;

   -------------------------------------------------------------------
   declare
      procedure Proc1 is new Proc;
   begin
      Proc1 (True);
   end;

   -------------------------------------------------------------------
   begin
      Raise_Exc (True);

   exception
      when others =>
         Failed ("EXCEPTIONS ARE DISTINCT FOR RECURSION - 13");
   end;

   -------------------------------------------------------------------

   Result;
end Cb1005a;

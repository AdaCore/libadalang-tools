-- CC3120B.ADA

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
-- CHECK THAT TASKS ARE NOT COPIED AS GENERIC IN OUT PARMS.

-- DAT 8/27/81
-- SPS 4/6/82
-- JBG 3/23/83

with Report; use Report;

procedure Cc3120b is
begin
   Test ("CC3120B", "TASKS ARE NOT COPIED AS GENERIC PARAMETERS");

   declare
      package P is
         type T is limited private;
         procedure Updt (Tparm : in T; I : in out Integer);
      private
         task type T1 is
            entry Get (I : out Integer);
            entry Put (I : in Integer);
         end T1;
         type T is record
            C : T1;
         end record;
      end P;
      use P;
      Tt : T;
      generic
         type T is limited private;
         T1 : in out T;
         with procedure Updt (Tparm : in T; I : in out Integer) is <>;
      procedure Pr;

      procedure Pr is
         I : Integer;
      begin
         I := 5;
         -- PR.I
         -- UPDT.I      UPDT.T1.I
         --   5            4
         Updt (T1, I);
         --   4            5
         if I /= 4 then
            Failed ("BAD VALUE 1");
         end if;
         I := 6;
         --   6            5
         Updt (T1, I);
         --   5            6
         if I /= 5 then
            Failed ("BAD VALUE 3");
         end if;
         raise Tasking_Error;
         Failed ("INCORRECT RAISE STATEMENT");
      end Pr;

      package body P is
         procedure Updt (Tparm : in T; I : in out Integer) is
            V : Integer := I;
            -- UPDT.I => V
            -- T1.I => UPDT.I
            -- V => T1.I
         begin
            Tparm.C.Get (I);
            Tparm.C.Put (V);
         end Updt;

         task body T1 is
            I : Integer;
         begin
            I := 1;
            loop
               select
                  accept Get (I : out Integer) do
                     I := T1.I;
                  end Get;
               or
                  accept Put (I : in Integer) do
                     T1.I := I;
                  end Put;
               or
                  terminate;
               end select;
            end loop;
         end T1;
      end P;
   begin
      declare
         X : Integer := 2;
         procedure Ppp is new Pr (T, Tt);
      begin
         -- X
         -- UPDT.I      UPDT.T1.I
         --   2            1
         Updt (Tt, X);
         --   1            2
         X := X + 3;
         --   4            2
         Updt (Tt, X);
         --   2            4
         if X /= 2 then
            Failed ("WRONG VALUE FOR X");
         end if;
         begin
            Ppp;
            Failed ("PPP NOT CALLED");
         exception
            when Tasking_Error =>
               null;
         end;
         X := 12;
         --   12           6
         Updt (Tt, X);
         --   6            12
         if X /= 6 then
            Failed ("WRONG FINAL VALUE IN TASK");
         end if;
      end;
   end;

   Result;
end Cc3120b;

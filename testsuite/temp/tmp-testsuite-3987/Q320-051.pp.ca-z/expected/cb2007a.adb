-- CB2007A.ADA

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
-- CHECK THAT AN EXIT STATEMENT IN A HANDLER CAN TRANSFER CONTROL
-- OUT OF A LOOP.

-- DAT 4/13/81
--  RM 4/30/81
-- SPS 3/23/83

with Report; use Report;

procedure Cb2007a is
begin
   Test ("CB2007A", "EXIT STATEMENTS IN EXCEPTION HANDLERS");

   declare
      Flow_Index : Integer := 0;
   begin

      for I in 1 .. 10 loop
         begin
            if I = 1 then
               raise Constraint_Error;
            end if;
            Failed ("WRONG CONTROL FLOW 1");
         exception
            when Constraint_Error =>
               exit;
         end;
         Failed ("WRONG CONTROL FLOW 2");
         exit;
      end loop;

      for Aaa in 1 .. 1 loop
         for Bbb in 1 .. 1 loop
            for I in 1 .. 10 loop
               begin
                  if I = 1 then
                     raise Constraint_Error;
                  end if;
                  Failed ("WRONG CONTROL FLOW A1");
               exception
                  when Constraint_Error =>
                     exit;
               end;
               Failed ("WRONG CONTROL FLOW A2");
               exit;
            end loop;

            Flow_Index := Flow_Index + 1;
         end loop;
      end loop;

      Loop1 :
      for Aaa in 1 .. 1 loop
         Loop2 :
         for Bbb in 1 .. 1 loop
            Loop3 :
            for I in 1 .. 10 loop
               begin
                  if I = 1 then
                     raise Constraint_Error;
                  end if;
                  Failed ("WRONG CONTROL FLOW B1");
               exception
                  when Constraint_Error =>
                     exit Loop2;
               end;
               Failed ("WRONG CONTROL FLOW B2");
               exit Loop2;
            end loop Loop3;

            Failed ("WRONG CONTROL FLOW B3");
         end loop Loop2;

         Flow_Index := Flow_Index + 1;
      end loop Loop1;

      if Flow_Index /= 2 then
         Failed ("WRONG FLOW OF CONTROL");
      end if;

   end;

   Result;
end Cb2007a;

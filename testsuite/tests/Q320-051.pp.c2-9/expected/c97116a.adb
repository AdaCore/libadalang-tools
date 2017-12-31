-- C97116A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE GUARD CONDITIONS IN A SELECTIVE WAIT STATEMENT
--     ARE NOT RE-EVALUATED DURING THE WAIT.

-- HISTORY:
--     WRG 7/10/86  CREATED ORIGINAL TEST.
--     RJW 5/15/90  REMOVED SHARED VARIABLES.

with Report; use Report;
procedure C97116a is

   Guard_Evaluations : Natural := 0;

   function Guard return Boolean is
   begin
      Guard_Evaluations := Guard_Evaluations + 1;
      return False;
   end Guard;

   function So_Long return Duration is
   begin
      return 20.0;
   end So_Long;

begin

   Test
     ("C97116A",
      "CHECK THAT THE GUARD CONDITIONS IN A " &
      "SELECTIVE WAIT STATEMENT ARE NOT RE-EVALUATED " & "DURING THE WAIT");

   declare

      task T is
         entry E;
      end T;

      task body T is
      begin
         select
            accept E;
            Failed ("ACCEPTED NONEXISTENT CALL TO E");
         or when Guard =>
            delay 0.0;
            Failed
              ("EXECUTED ALTERNATIVE CLOSED BY FALSE " & "GUARD FUNCTION");
         or
            delay So_Long;
         end select;
      exception
         when others =>
            Failed ("EXCEPTION RAISED");
      end T;

      task Get_Cpu;

      task body Get_Cpu is
      begin
         while not T'Terminated loop
            delay 1.0;
         end loop;

      end Get_Cpu;

   begin

      null;

   end;

   if Guard_Evaluations /= 1 then
      Failed
        ("GUARD EVALUATED" & Natural'Image (Guard_Evaluations) & " TIMES");
   end if;

   Result;

end C97116a;

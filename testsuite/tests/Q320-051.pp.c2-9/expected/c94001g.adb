-- C94001G.ADA

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
-- CHECK THAT A COMPLETED TASK WITH DEPENDENT TASKS TERMINATES WHEN A L L
-- DEPENDENT TASKS HAVE TERMINATED.

-- WEI  3/ 4/82
-- JBG 4/2/84
-- JWC 6/28/85 RENAMED FROM C940AIA-B.ADA

with Report; use Report;
procedure C94001g is

   package Spy is      -- PROVIDE PROTECTED ACCESS TO SPYNUMB
      subtype Arg is Natural range 0 .. 9;
      function Spynumb return Natural;                      -- READ
      function Finit_Pos (Digt : in Arg) return Natural;    -- WRITE
      procedure Pspy_Numb (Digt : in Arg);                  -- WRITE
   end Spy;

   use Spy;

   package body Spy is

      task Guard is
         entry Read (Numb : out Natural);
         entry Write (Numb : in Natural);
      end Guard;

      task body Guard is
         Spynumb : Natural := 0;
      begin
         loop
            select
               accept Read (Numb : out Natural) do
                  Numb := Spynumb;
               end Read;
            or
               accept Write (Numb : in Natural) do
                  Spynumb := 10 * Spynumb + Numb;
               end Write;
            or
               terminate;
            end select;
         end loop;
      end Guard;

      function Spynumb return Natural is
         Temp : Natural;
      begin
         Guard.Read (Temp);
         return Temp;
      end Spynumb;

      function Finit_Pos (Digt : in Arg) return Natural is
      begin
         Guard.Write (Digt);
         return Digt;
      end Finit_Pos;

      procedure Pspy_Numb (Digt : in Arg) is
      begin
         Guard.Write (Digt);
      end Pspy_Numb;
   end Spy;

begin
   Test
     ("C94001G",
      "TERMINATION WHEN ALL DEPENDENT TASKS " & "HAVE TERMINATED");

   Block :
   declare

      task type Tt1;

      task body Tt1 is
      begin
         delay 1.0;
         Pspy_Numb (1);
      end Tt1;

      task T1 is
      end T1;

      task body T1 is
         Obj_Tt1_1, Obj_Tt1_2, Obj_Tt1_3 : Tt1;
      begin
         null;
      end T1;

   begin
      null;
   end Block;               -- WAIT HERE FOR TERMINATION.

   if Spynumb /= 111 then
      Failed
        ("TASK T1 TERMINATED BEFORE " & "ALL DEPENDENT TASKS HAVE TERMINATED");
      Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
   end if;

   Result;

end C94001g;

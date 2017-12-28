-- C93008A.ADA

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
-- CHECK THAT FOR A TASK CREATED BY AN OBJECT DECLARATION, EXECUTION DOES NOT
-- PROCEED IN PARALLEL WITH ACTIVATION.

-- R.WILLIAMS 8/20/86

with Report; use Report;
procedure C93008a is

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   task T is
      entry Finit_Pos (Digt : in Arg);
   end T;

   task body T is
   begin
      loop
         select
            accept Finit_Pos (Digt : in Arg) do
               Spynumb := 10 * Spynumb + Digt;
            end Finit_Pos;
         or
            terminate;
         end select;
      end loop;
   end T;

begin

   Test
     ("C93008A",
      "CHECK THAT EXECUTION DOES NOT PROCEED IN  " &
      "PARALLEL WITH ACTIVATION OF A TASK CREATED " &
      "BY AN OBJECT DECLARATION");

   Block :
   declare

      task type Tt1;

      task Tt2;

      T1 : Tt1;

      task body Tt1 is
         package Dummy is
         end Dummy;

         package body Dummy is
         begin
            delay 2.0;
            T.Finit_Pos (1);
         end Dummy;
      begin
         null;
      end Tt1;

      task body Tt2 is
         package Dummy is
         end Dummy;

         package body Dummy is
         begin
            delay 2.0;
            T.Finit_Pos (2);
         end Dummy;
      begin
         null;
      end Tt2;

   begin               -- TASKS ACTIVATED NOW.

      if Spynumb = 12 or Spynumb = 21 then
         null;
      else
         Failed
           ("TASKS NOT ACTIVATED PROPERLY - SPYNUMB HAS " &
            "ACTUAL VALUE OF: " &
            Integer'Image (Spynumb));
      end if;
   end Block;

   Result;

end C93008a;

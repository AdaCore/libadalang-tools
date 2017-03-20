-- CC3607B.ADA

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
--     CHECK THAT WHEN A DEFAULT SUBPROGRAM IS SPECIFIED WITH A BOX, A
--     SUBPROGRAM DIRECTLY VISIBLE AT THE POINT OF INSTANTIATION
--     IS USED.

-- HISTORY:
--     LDC 08/23/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cc3607b is

begin
   Test
     ("CC3607B",
      "CHECK THAT WHEN A DEFAULT SUBPROGRAM IS " &
      "SPECIFIED WITH A BOX, A SUBPROGRAM DIRECTLY " &
      "VISIBLE AT THE POINT OF INSTANTIATION IS USED");
   declare
      package Proc_Pack is
         procedure Proc;

         generic
            with procedure Proc is <>;
         package Gen_Pack is
            procedure Do_Proc;
         end Gen_Pack;
      end Proc_Pack;
      use Proc_Pack;

      package body Proc_Pack is
         procedure Proc is
         begin
            Failed ("WRONG SUBPROGRAM WAS USED");
         end Proc;

         package body Gen_Pack is
            procedure Do_Proc is
            begin
               Proc;
            end Do_Proc;
         end Gen_Pack;
      end Proc_Pack;

      procedure Proc is
      begin
         Comment ("SUBPROGRAM VISIBLE AT INSTANTIATION WAS " & "USED");
      end Proc;

      package New_Pack is new Gen_Pack;

   begin
      New_Pack.Do_Proc;
   end;

   Result;
end Cc3607b;

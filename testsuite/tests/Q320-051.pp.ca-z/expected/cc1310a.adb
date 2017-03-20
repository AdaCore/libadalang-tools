-- CC1310A.ADA

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
-- CHECK THAT DEFAULT GENERIC SUBPROGRAM PARAMETERS MAY BE ENTRIES.

-- DAT 9/8/81
-- SPS 2/7/83

with Report; use Report;

procedure Cc1310a is
begin
   Test
     ("CC1310A",
      "DEFAULT GENERIC SUBPROGRAM PARAMETERS MAY BE" & " ENTRIES");

   declare
      task T is
         entry Ent1;
         entry Ent2 (I : in Integer);
      end T;

      procedure P1 renames T.Ent1;

      procedure P4 (I : in Integer) renames T.Ent2;

      Int : Integer := 0;

      task body T is
      begin
         accept Ent1;
         accept Ent2 (I : in Integer) do
            Int := Int + I;
         end Ent2;
         accept Ent2 (I : in Integer) do
            Int := Int + I;
         end Ent2;
         accept Ent1;
      end T;

   begin
      declare
         generic
            with procedure P1 is <>;
            with procedure P2 is T.Ent1;
            with procedure P3 (I : in Integer) is T.Ent2;
            with procedure P4 (I : in Integer) is <>;
         package Pkg is
         end Pkg;

         package body Pkg is
         begin
            P1;
            P4 (3);
            P3 (6);
            P2;
         end Pkg;

         package Pp is new Pkg;

      begin
         if Int /= 9 then
            Failed ("ENTRIES AS DEFAULT GENERIC PARAMETERS");
         end if;
      end;
   end;

   Result;
end Cc1310a;

-- CC3602A.ADA

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
--     CHECK THAT ENTRIES MAY BE PASSED AS GENERIC SUBPROGRAM
--     PARAMETERS.

-- HISTORY:
--     DAT 9/25/81     CREATED ORIGINAL TEST.
--     LDC 10/6/88     REVISED; CHECKED THAT DEFAULT NAME CAN BE
--                     IDENTIFIED WITH ENTRY.

with Report; use Report;

procedure Cc3602a is
   Counter : Integer := 0;
begin
   Test ("CC3602A", "ENTRIES AS GENERIC SUBPROGRAM PARAMETERS");

   declare
      task Tsk is
         entry Ent;
      end Tsk;

      generic
         with procedure P;
      procedure Gp;

      generic
         with procedure P;
      package Pk is
      end Pk;

      procedure E1 renames Tsk.Ent;

      generic
         with procedure P is Tsk.Ent;
      procedure Gp_Def1;

      generic
         with procedure P is E1;
      procedure Gp_Def2;

      generic
         with procedure P is Tsk.Ent;
      package Pk_Def1 is
      end Pk_Def1;

      generic
         with procedure P is E1;
      package Pk_Def2 is
      end Pk_Def2;

      procedure Gp is
      begin
         P;
      end Gp;

      package body Pk is
      begin
         P;
      end Pk;

      procedure Gp_Def1 is
      begin
         P;
      end Gp_Def1;

      procedure Gp_Def2 is
      begin
         P;
      end Gp_Def2;

      package body Pk_Def1 is
      begin
         P;
      end Pk_Def1;

      package body Pk_Def2 is
      begin
         P;
      end Pk_Def2;

      task body Tsk is
      begin
         loop
            select
               accept Ent do
                  Counter := Counter + 1;
               end Ent;
            or
               terminate;
            end select;
         end loop;
      end Tsk;

   begin
      declare
         procedure P1 is new Gp (Tsk.Ent);
         procedure E renames Tsk.Ent;
         procedure P2 is new Gp (E);
         package Pk1 is new Pk (Tsk.Ent);
         package Pk2 is new Pk (E);

         procedure P3 is new Gp_Def1;
         procedure P4 is new Gp_Def2;
         package Pk3 is new Pk_Def1;
         package Pk4 is new Pk_Def2;
      begin
         P1;
         P2;
         Tsk.Ent;
         E;
         P3;
         P4;
      end;
      Tsk.Ent;
   end;

   if Counter /= 11 then
      Failed ("INCORRECT CALL OF ENTRY AS GENERIC PARAMETER");
   end if;

   Result;
end Cc3602a;

-- CC1004A.ADA

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
--     CHECK THAT THE ELABORATION OF A GENERIC DECLARATION
--     DOES NOT ELABORATE THE SUBPROGRAM OR PACKAGE SPECIFICATION.

-- HISTORY:
--     DAT 07/31/81  CREATED ORIGINAL TEST.
--     SPS 10/18/82
--     SPS 02/09/83
--     JET 01/07/88  UPDATED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.

with Report; use Report;

procedure Cc1004a is
begin
   Test
     ("CC1004A",
      "THE SPECIFICATION PART OF A GENERIC " &
      "SUBPROGRAM IS NOT ELABORATED AT THE " &
      "ELABORATION OF THE DECLARATION");

   begin
      declare
         subtype I1 is Integer range 1 .. 1;

         generic
         procedure Proc (P1 : I1 := Ident_Int (2));

         procedure Proc (P1 : I1 := Ident_Int (2)) is
         begin
            if not Equal (P1, P1) then
               Comment ("DON'T OPTIMIZE THIS");
            end if;
         end Proc;
      begin
         begin
            declare
               procedure P is new Proc;
            begin
               if not Equal (3, 3) then
                  P (1);
               end if;
            end;
         exception
            when others =>
               Failed ("INSTANTIATION ELABORATES SPEC");
         end;

      end;
   exception
      when others =>
         Failed ("DECL ELABORATED SPEC PART - 1");
   end;

   begin
      declare
         subtype I1 is Integer range 1 .. 1;

         generic
         package Pkg is
            X : Integer := I1 (Ident_Int (2));
         end Pkg;
      begin
         begin
            declare
               package P is new Pkg;
            begin
               Failed ("PACKAGE INSTANTIATION FAILED");
               if not Equal (P.X, P.X) then
                  Comment ("DON'T OPTIMIZE THIS");
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION - 2");
         end;

      end;
   exception
      when others =>
         Failed ("DECL ELABORATED SPEC PART - 2");
   end;

   Result;

end Cc1004a;

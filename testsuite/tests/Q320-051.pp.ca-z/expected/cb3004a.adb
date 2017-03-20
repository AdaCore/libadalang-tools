-- CB3004A.ADA

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
-- CHECK THAT WHEN AN INNER UNIT REDECLARES AN EXCEPTION NAME
--    THE HIDDEN DEFINITION IS STILL AVAILABLE FOR USE.

--  NOTE : WE ASSUME FUNCTIONS ACT LIKE PROCEDURES AND
--  THAT UNITS, BLOCKS, AND PROCEDURES ACT THE SAME
--  IN OTHER CONTEXTS (E.G. TASKS AND PACKAGES).

-- DCB 6/2/80
-- JRK 11/19/80
-- SPS 3/24/83

with Report;
procedure Cb3004a is

   use Report;

   E1 : exception;
   Flow_Count : Integer := 0;

   procedure P1 is
      E1, E2 : exception;

      procedure P2 is
         E1 : exception;
      begin
         Flow_Count := Flow_Count + 1;
         raise E1;
         Failed ("E1 EXCEPTION NOT RAISED");
      exception
         when P1.E1 =>
            Failed ("P1.E1 EXCEPTION RAISED WHEN " & "(P2)E1 EXPECTED");
         when E1 =>
            begin
               Flow_Count := Flow_Count + 1;
               raise P1.E1;
               Failed ("P1.E1 EXCEPTION NOT RAISED");
            exception
               when E1 =>
                  Failed ("(P2)E1 EXCEPTION RAISED WHEN" & " P1.E1 EXPECTED");
               when P1.E1 =>
                  Flow_Count := Flow_Count + 1;
               when others =>
                  Failed ("OTHERS RAISED WHEN P1.E1 " & "EXPECTED");
            end;
         when others =>
            Failed ("OTHERS RAISED WHEN (P2)E1 EXPECTED");
      end P2;

      procedure P3 is
         Constraint_Error : exception;
      begin
         Flow_Count := Flow_Count + 1;
         raise Constraint_Error;
         Failed ("CONSTRAINT_ERROR EXCEPTION NOT RAISED");
      exception
         when Standard.Constraint_Error =>
            Failed
              ("STANDARD.CONSTRAINT_ERROR EXCEPTION " &
               "RAISED WHEN " &
               "(P3)CONSTRAINT_ERROR EXPECTED");
         when Constraint_Error =>
            begin
               Flow_Count := Flow_Count + 1;
               raise Standard.Constraint_Error;
               Failed ("STANDARD.CONSTRAINT_ERROR " & "EXCEPTION NOT RAISED");
            exception
               when Constraint_Error =>
                  Failed
                    ("(P3)CONSTRAINT_ERROR " &
                     "EXCEPTION RAISED WHEN " &
                     "STANDARD.CONSTRAINT_ERROR " &
                     "EXPECTED");
               when Standard.Constraint_Error =>
                  Flow_Count := Flow_Count + 1;
               when others =>
                  Failed
                    ("OTHERS RAISED WHEN " &
                     "STANDARD.CONSTRAINT_ERROR " &
                     "EXPECTED");
            end;
         when others =>
            Failed ("OTHERS RAISED WHEN " & "(P3)CONSTRAINT_ERROR EXPECTED");
      end P3;

      procedure P4 is
         E2 : exception;
      begin
         Flow_Count := Flow_Count + 1;
         raise P1.E2;
         Failed ("P1.E2 EXCEPTION NOT RAISED");
      exception
         when E2 =>
            Failed ("(P4).E2 RAISED WHEN P1.E2 EXPECTED");
      end P4;

   begin  -- P1
      P2;
      P3;
      P4;
      Failed ("P1.E2 EXCEPTION NOT PROPAGATED FROM P4");
   exception
      when E2 =>
         Flow_Count := Flow_Count + 1;
      when others =>
         Failed ("EXCEPTION RAISED WHERE NONE EXPECTED");
   end P1;

begin
   Test
     ("CB3004A",
      "CHECK THAT WHEN EXCEPTION NAMES" &
      " ARE REDECLARED THE HIDDEN DEFINITION IS STILL AVAILABLE");

   P1;

   if Flow_Count /= 8 then
      Failed ("INCORRECT FLOW_COUNT VALUE");
   end if;

   Result;
end Cb3004a;

-- C37217C.ADA

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
--     CHECK WHETHER THE OPTIONAL COMPATIBILITY CHECK IS
--     PERFORMED WHEN A DISCRIMINANT CONSTRAINT IS GIVEN FOR AN ACCESS
--     TYPE - WHEN THERE IS A "LOOP" IN THE DESIGNATED TYPE'S FULL
--     DECLARATION.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C37217c is

begin  --C37217C BODY
   Test
     ("C37217C",
      "CHECK WHETHER THE OPTIONAL COMPATIBILITY " &
      "CHECK IS PERFORMED WHEN A DISCRIMINANT " &
      "CONSTRAINT IS GIVEN FOR AN ACCESS TYPE " &
      "- WHEN THERE IS A ""LOOP"" IN THE DESIGNATED " &
      "TYPE'S FULL DECLARATION");

   begin
      declare
         type R1 (D1 : Integer);
         type R2 (D2 : Integer);
         type R3 (D3 : Positive);

         type Acc_R1 is access R1;
         type Acc_R2 is access R2;
         type Acc_R3 is access R3;

         type R1 (D1 : Integer) is record
            C1 : Acc_R2 (D1);
         end record;

         type R2 (D2 : Integer) is record
            C2 : Acc_R3 (D2);
         end record;

         type R3 (D3 : Positive) is record
            C3 : Acc_R1 (D3);
         end record;

         X1 : Acc_R1 (Ident_Int (0));

      begin
         Comment ("OPTIONAL COMPATIBILITY CHECK NOT PERFORMED");

         X1 :=
           new R1'
             (D1 => Ident_Int (0),
              C1 =>
                new R2'(D2 => Ident_Int (0), C2 => new R3 (Ident_Int (0))));

         Failed ("CONSTRAINT_ERROR NOT RAISED");

         if Ident_Int (X1.C1.C2.D3) /= Ident_Int (0) then
            Comment ("THIS LINE SHOULD NOT PRINT OUT");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED IN " & "VARIABLE USE - LOOPED");
      end;
   exception
      when Constraint_Error =>
         Comment ("OPTIONAL COMPATIBILITY CHECK PERFORMED");
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED IN " &
            "VARIABLE DECLARATION - LOOPED");
   end;

   Result;

end C37217c;  -- BODY

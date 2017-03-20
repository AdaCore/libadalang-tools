-- C41304A.ADA

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
--     CHECK THAT L.R RAISES CONSTRAINT_ERROR WHEN:
--       L DENOTES AN ACCESS OBJECT HAVING THE VALUE NULL.
--       L IS A FUNCTION CALL DELIVERING THE ACCESS VALUE NULL.

-- HISTORY:
--     WKB 08/14/81
--     JRK 08/17/81
--     SPS 10/26/82
--     TBN 03/26/86  PUT THE NON-EXISTENT COMPONENT CASES INTO C41304B.
--     JET 01/05/88  MODIFIED HEADER FORMAT AND ADDED CODE TO PREVENT
--                   OPTIMIZATION.

with Report; use Report;
procedure C41304a is

   type R is record
      I : Integer;
   end record;

   type T is access R;

begin
   Test
     ("C41304A",
      "CONSTRAINT_ERROR WHEN L IN L.R DENOTES A NULL " &
      "ACCESS OBJECT OR A FUNCTION CALL DELIVERING " &
      "NULL");

   --------------------------------------------------

   declare

      A : T := new R'(I => 1);
      J : Integer;

   begin

      if Equal (4, 4) then
         A := null;
      end if;

      J := A.I;
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR A NULL ACCESS " & "OBJECT");

      if Equal (J, J) then
         Comment ("NO EXCEPTION RAISED");
      end if;

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR A NULL ACCESS " & "OBJECT");

   end;

   --------------------------------------------------

   declare

      J : Integer;

      function F return T is
      begin
         if Equal (4, 4) then
            return null;
         end if;
         return new R'(I => 2);
      end F;

   begin

      J := F.I;
      Failed
        ("CONSTRAINT_ERROR NOT RAISED FOR A FUNCTION CALL " &
         "DELIVERING A NULL ACCESS VALUE");

      if Equal (J, J) then
         Comment ("NO EXCEPTION RAISED");
      end if;

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR A FUNCTION CALL " &
            "DELIVERING A NULL ACCESS VALUE");

   end;

   Result;
end C41304a;

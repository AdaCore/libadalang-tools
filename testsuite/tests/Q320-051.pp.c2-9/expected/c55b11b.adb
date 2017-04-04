-- C55B11B.ADA

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
--     CHECK THAT THE FORM 'FOR I IN ST RANGE L .. R LOOP' IS ACCEPTED
--     EVEN IF BOTH L AND R ARE OVERLOADED ENUMERATION LITERALS (SO
--     THAT L .. R WOULD BE ILLEGAL WITHOUT ST RANGE).

-- HISTORY:
--     DHH 09/07/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C55b11b is
   type St is (A, B, C, D, E, F, G, H);
   type Si is (A, B, C, D, F, E, G, H);

   Global : Integer := 0;

   procedure Check_Var (T : St) is
   begin
      Global := Global + 1;
      case T is
         when D =>
            if Global /= Ident_Int (1) then
               Failed ("1 WRONG VALUE");
            end if;

         when E =>
            if Global /= Ident_Int (2) then
               Failed ("2 WRONG VALUE");
            end if;

         when F =>
            if Global /= Ident_Int (3) then
               Failed ("3 WRONG VALUE");
            end if;

         when G =>
            if Global /= Ident_Int (4) then
               Failed ("4 WRONG VALUE");
            end if;

         when others =>
            Failed ("WRONG VALUE TO PROCEDURE");

      end case;
   end Check_Var;

   procedure Check_Var (T : Si) is
   begin
      Failed ("WRONG PROCEDURE CALLED");
   end Check_Var;

begin
   Test
     ("C55B11B",
      "CHECK THAT THE 'FORM FOR I IN ST RANGE L .. R " &
      "LOOP' IS ACCEPTED EVEN IF BOTH L AND R ARE " &
      "OVERLOADED ENUMERATION LITERALS (SO THAT L .. " &
      "R WOULD BE ILLEGAL WITHOUT ST RANGE)");

   for I in St range D .. G loop
      Check_Var (I);
   end loop;

   Result;
end C55b11b;

-- C55B11A.ADA

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
--     CHECK THAT, IN 'FOR IN ST RANGE L .. R LOOP', THE PARAMETER IS OF
--     THE TYPE ST'BASE; THAT IS THAT IT CAN BE ASSIGNED TO OTHER
--     VARIABLES DECLARED WITH SOME OTHER SUBTYPES OF ST.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C55b11a is

   type Enum is (A, B, C, D, E, F, G, H);

   subtype One is Enum range A .. H;
   subtype Two is Enum range B .. H;
   subtype Three is Enum range C .. H;
   subtype Four is Enum range D .. H;

   Global : Integer := 0;

   Var_1 : One;
   Var_2 : Two;
   Var_3 : Three;
   Var_4 : Four;

   procedure Check_Var (T : Enum) is
   begin
      Global := Global + 1;
      case T is
         when D =>
            if Global /= Ident_Int (1) then
               Failed ("VAR_1 WRONG VALUE");
            end if;

         when E =>
            if Global /= Ident_Int (2) then
               Failed ("VAR_2 WRONG VALUE");
            end if;

         when F =>
            if Global /= Ident_Int (3) then
               Failed ("VAR_3 WRONG VALUE");
            end if;

         when G =>
            if Global /= Ident_Int (4) then
               Failed ("VAR_4 WRONG VALUE");
            end if;

         when others =>

            Failed ("WRONG VALUE TO PROCEDURE");
      end case;
   end Check_Var;

begin
   Test
     ("C55B11A",
      "CHECK THAT, IN 'FOR IN ST RANGE L .. R LOOP', " &
      "THE PARAMETER IS OF THE TYPE ST'BASE; THAT IS " &
      "THAT IT CAN BE ASSIGNED TO OTHER VARIABLES " &
      "DECLARED WITH SOME OTHER SUBTYPES OF ST");

   for I in One range D .. G loop
      case I is
         when D =>
            Var_1 := I;
            Check_Var (Var_1);
         when E =>
            Var_2 := I;
            Check_Var (Var_2);
         when F =>
            Var_3 := I;
            Check_Var (Var_3);
         when G =>
            Var_4 := I;
            Check_Var (Var_4);
      end case;
   end loop;

   Result;
end C55b11a;

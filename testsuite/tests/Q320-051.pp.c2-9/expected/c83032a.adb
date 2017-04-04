-- C83032A.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR OR
--     AN ENUMERATION LITERAL IS HIDDEN BY A DERIVED SUBPROGRAM
--     HOMOGRAPH.

-- HISTORY:
--     VCL  08/10/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83032a is
begin
   Test
     ("C83032A",
      "AN IMPLICIT DECLARATION OF A PREDEFINED " &
      "OPERATOR OR AN ENUMERATION LITERAL IS HIDDEN " &
      "BY A DERIVED SUBPROGRAM HOMOGRAPH");

   declare             -- CHECK PREDEFINED OPERATOR.
      package P is
         type Int is range -20 .. 20;
         function "ABS" (X : Int) return Int;
      end P;
      use P;

      type Nint is new Int;

      I2 : Nint := -5;

      package body P is
         I1 : Nint := 5;

         function "ABS" (X : Int) return Int is
         begin
            return Int (-(abs (Integer (X))));
         end "ABS";

      begin
         if "ABS" (I1) /= -5 then
            Failed
              ("INCORRECT VALUE FOR 'I1' AFTER CALL " &
               "TO DERIVED ""ABS"" - 1");
         end if;

         I1 := abs (-10);
         if abs I1 /= Nint (Ident_Int (-10)) then
            Failed
              ("INCORRECT VALUE FOR 'I1' AFTER CALL " &
               "TO DERIVED ""ABS"" - 2");
         end if;
      end P;
   begin
      if "ABS" (I2) /= -5 then
         Failed
           ("INCORRECT VALUE FOR 'I2' AFTER CALL " & "TO DERIVED ""ABS"" - 1");
      end if;

      I2 := abs (10);
      if abs I2 /= Nint (Ident_Int (-10)) then
         Failed
           ("INCORRECT VALUE FOR 'I1' AFTER CALL " & "TO DERIVED ""ABS"" - 2");
      end if;
   end;

   declare   -- CHECK ENUMERATION LITERALS.

      package P1 is
         type Enum1 is (E11, E12, E13);
         type Priv1 is private;
         function E11 return Priv1;
      private
         type Priv1 is new Enum1;
         type Npriv1 is new Priv1;
      end P1;
      use P1;

      package body P1 is
         function E11 return Priv1 is
         begin
            return E13;
         end E11;
      begin
         if Npriv1'(E11) /= E13 then
            Failed ("INCORRECT VALUE FOR E11");
         end if;
      end P1;

   begin
      null;
   end;
   Result;
end C83032a;

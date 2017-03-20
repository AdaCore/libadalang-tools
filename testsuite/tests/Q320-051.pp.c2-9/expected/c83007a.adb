-- C83007A.ADA

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
--     CHECK THAT A FORMAL PARAMETER OF A SUBPROGRAM DECLARED BY A
--     RENAMING DECLARATION CAN HAVE THE SAME IDENTIFIER AS A
--     DECLARATION IN THE BODY OF THE RENAMED SUBPROGRAM.

-- HISTORY:
--     VCL  02/18/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83007a is
begin
   Test
     ("C83007A",
      "A FORMAL PARAMETER OF A SUBPROGRAM DECLARED " &
      "BY A RENAMING DECLARATION CAN HAVE THE SAME " &
      "IDENTIFIER AS A DECLARATION IN THE BODY OF " &
      "THE RENAMED SUBPROGRAM");
   declare
      procedure P (One : Integer; Two : Float; Three : String);

      procedure R (D1 : Integer; D2 : Float; D3 : String) renames P;

      procedure P (One : Integer; Two : Float; Three : String) is
         type D1 is range 1 .. 10;
         I : D1 := D1 (Ident_Int (7));

         D2 : Float;

         function D3 return String is
         begin
            return "D3";
         end D3;

         function Ident_Float (Val : Float) return Float is
         begin
            if Equal (3, 3) then
               return Val;
            else
               return 0.0;
            end if;
         end Ident_Float;

      begin
         if One /= 5 then
            Failed ("INCORRECT VALUE FOR PARAMETER ONE");
         end if;
         if Two /= 4.5 then
            Failed ("INCORRECT VALUE FOR PARAMETER TWO");
         end if;
         if Three /= "R1" then
            Failed ("INCORRECT VALUE FOR PARAMETER THREE");
         end if;

         if I /= 7 then
            Failed ("INCORRECT VALUE FOR OBJECT I");
         end if;
         D2 := Ident_Float (3.5);
         if D2 /= 3.5 then
            Failed ("INCORRECT VALUE FOR OBJECT D2");
         end if;
         if D3 /= "D3" then
            Failed ("INCORRECT VALUE FOR FUNCTION D3");
         end if;
      end P;
   begin
      R (D1 => 5, D2 => 4.5, D3 => "R1");
   end;

   Result;
end C83007a;

-- C41309A.ADA

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
-- CHECK THAT AN EXPANDED NAME IS ALLOWED EVEN IF A USE CLAUSE MAKES THE
-- EXPANDED NAME UNNECESSARY.

-- TBN  12/15/86

with Report; use Report;
procedure C41309a is

begin
   Test
     ("C41309A",
      "CHECK THAT AN EXPANDED NAME IS ALLOWED EVEN " &
      "IF A USE CLAUSE MAKES THE EXPANDED NAME " &
      "UNNECESSARY");
   declare
      package P is
         package Q is
            package R is
               type Rec is record
                  A : Integer := 5;
                  B : Boolean := True;
               end record;
               Rec1 : Rec;
            end R;

            use R;

            Rec2 : R.Rec := R.Rec1;
         end Q;

         use Q;
         use R;

         Rec3 : Q.R.Rec := Q.Rec2;
      end P;

      use P;
      use Q;
      use R;

      Rec4 : P.Q.R.Rec := P.Rec3;
   begin
      if Rec4 /= (Ident_Int (5), Ident_Bool (True)) then
         Failed ("INCORRECT RESULTS FROM EXPANDED NAME");
      end if;
   end;

   Result;
end C41309a;

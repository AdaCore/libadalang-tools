-- C95093A.ADA

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
-- CHECK THAT THE DEFAULT EXPRESSIONS OF FORMAL PARAMETERS ARE EVALUATED
-- EACH TIME THEY ARE NEEDED.

-- GLH 7/2/85

with Report; use Report;

procedure C95093a is
begin

   Test
     ("C95093A",
      "CHECK THAT THE DEFAULT EXPRESSION IS " &
      "EVALUATED EACH TIME IT IS NEEDED");

   declare

      X : Integer := 1;

      function F return Integer is
      begin
         X := X + 1;
         return X;
      end F;

      task T1 is
         entry E1 (X, Y : Integer := F);
      end T1;

      task body T1 is
      begin

         accept E1 (X, Y : Integer := F) do
            if X = Y or Y /= 2 then
               Failed
                 ("DEFAULT NOT EVALUATED CORRECTLY - " &
                  "1, X =" &
                  Integer'Image (X) &
                  ", Y =" &
                  Integer'Image (Y));
            end if;
         end E1;

         accept E1 (X, Y : Integer := F) do
            if X = Y or not ((X = 3 and Y = 4) or (X = 4 and Y = 3)) then
               Failed
                 ("DEFAULT NOT EVALUATED CORRECTLY - " &
                  "2, X =" &
                  Integer'Image (X) &
                  ", Y =" &
                  Integer'Image (Y));
            end if;
         end E1;

      end T1;

   begin

      Comment ("FIRST CALL");
      T1.E1 (3);

      Comment ("SECOND CALL");
      T1.E1;

   end;

   Result;

end C95093a;

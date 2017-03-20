-- C95074C.ADA

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
-- CHECK THAT 'FIRST, 'LAST, 'LENGTH, AND 'RANGE, CAN BE APPLIED TO AN
-- OUT PARAMETER OR OUT PARAMETER SUBCOMPONENT THAT DOES NOT HAVE AN
-- ACCESS TYPE.

-- JWC 6/25/85

with Report; use Report;
procedure C95074c is

begin

   Test
     ("C95074C",
      "CHECK THAT ATTRIBUTES MAY BE APPLIED TO " &
      "NON-ACCESS FORMAL OUT PARAMETERS");

   declare

      type Arr is array (1 .. 10) of Natural;

      type Rec is record
         A : Arr;
      end record;

      A1 : Arr;
      R1 : Rec;

      task T1 is
         entry E (A2 : out Arr; R2 : out Rec);
      end T1;

      task body T1 is
      begin
         accept E (A2 : out Arr; R2 : out Rec) do

            if A2'First /= 1 then
               Failed ("WRONG VALUE FOR A2'FIRST");
            end if;

            if A2'Last /= 10 then
               Failed ("WRONG VALUE FOR A2'LAST");
            end if;

            if A2'Length /= 10 then
               Failed ("WRONG VALUE FOR A2'LENGTH");
            end if;

            if (1 not in A2'Range) or
              (10 not in A2'Range) or
              (0 in A2'Range) or
              (11 in A2'Range)
            then
               Failed ("WRONG VALUE FOR A2'RANGE");
            end if;

            if R2.A'First /= 1 then
               Failed ("WRONG VALUE FOR R2.A'FIRST");
            end if;

            if R2.A'Last /= 10 then
               Failed ("WRONG VALUE FOR R2.A'LAST");
            end if;

            if R2.A'Length /= 10 then
               Failed ("WRONG VALUE FOR R2.A'LENGTH");
            end if;

            if (1 not in R2.A'Range) or
              (10 not in R2.A'Range) or
              (0 in R2.A'Range) or
              (11 in R2.A'Range)
            then
               Failed ("WRONG VALUE FOR R2.A'RANGE");
            end if;
         end E;
      end T1;

   begin
      T1.E (A1, R1);
   end;

   Result;
end C95074c;

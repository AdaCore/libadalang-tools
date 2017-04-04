-- C23001A.ADA

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
-- CHECK THAT UPPER AND LOWER CASE LETTERS ARE EQUIVALENT IN IDENTIFIERS
-- (INCLUDING RESERVED WORDS).

-- JRK 12/12/79
-- JWC 6/28/85 RENAMED TO -AB

with Report;
procedure C23001a is

   use Report;

   An_Identifier : Integer := 1;

begin
   Test ("C23001A", "UPPER/LOWER CASE EQUIVALENCE IN IDENTIFIERS");

   declare
      An_Identifier : Integer := 3;
   begin
      if An_Identifier /= An_Identifier then
         Failed
           ("LOWER CASE NOT EQUIVALENT TO UPPER " &
            "IN DECLARABLE IDENTIFIERS");
      end if;
   end;

   if An_Identifier /= An_Identifier then
      Failed
        ("MIXED CASE NOT EQUIVALENT TO UPPER IN " & "DECLARABLE IDENTIFIERS");
   end if;

   if An_Identifier = 1 then
      An_Identifier := 2;
   end if;
   if An_Identifier /= 2 then
      Failed
        ("LOWER AND/OR MIXED CASE NOT EQUIVALENT TO " &
         "UPPER IN RESERVED WORDS");
   end if;

   Result;
end C23001a;

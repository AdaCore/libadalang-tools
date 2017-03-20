-- C37003B.ADA

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
--     CHECK THAT FOR A RECORD WITH MULTIPLE DISCRIMINANTS WHICH HAVE
--     DEFAULT EXPRESSIONS, THE EXPRESSIONS ARE EVALUATED ONCE FOR
--     EACH DISCRIMINANT IN THE ASSOCIATION.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C37003b is

   X : Integer := 0;

   function F1 return Integer is
   begin
      X := X + 1;
      return X;
   end F1;

begin
   Test
     ("C37003B",
      "CHECK THAT FOR A RECORD WITH MULTIPLE " &
      "DISCRIMINANTS WHICH HAVE DEFAULT EXPRESSIONS, " &
      "THE  EXPRESSIONS ARE EVALUATED ONCE FOR EACH " &
      "DISCRIMINANT IN THE ASSOCIATION");

   declare
      type Rec (D1, D2, D3, D4, D5 : Integer := F1) is record
         Y : Integer := (D1 + D2 + D3 + D4 + D5);
      end record;

      Rec_F1 : Rec;

   begin
      if Rec_F1.Y /= Ident_Int (15) then
         Failed ("MULTIPLE DISCRIMINANTS NOT EVALUATED " & "SEPARATELY");
      end if;
   end;

   Result;
end C37003b;

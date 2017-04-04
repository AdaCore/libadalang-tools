-- C37206A.ADA

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
--     FOR A TYPE WITHOUT DEFAULT DISCRIMINANT VALUES (BUT WITH
--     DISCRIMINANTS) CHECK THAT A TYPEMARK WHICH DENOTES SUCH AN
--     UNCONSTRAINED TYPE CAN BE USED IN:

--      1) A SUBTYPE DECLARATION, AND THE SUBTYPE NAME ACTS SIMPLY AS A
--         NEW NAME FOR THE UNCONSTRAINED TYPE;
--      2) IN A CONSTANT DECLARATION.

-- HISTORY:
--     AH  08/21/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     DTN 11/13/91 DELETED SUBPARTS (2 and 3).

with Report; use Report;
procedure C37206a is
begin

   Test
     ("C37206A",
      "FOR TYPE WITH DEFAULT-LESS DISCRIMINANTS, " &
      "UNCONSTRAINED TYPE_MARK CAN BE USED IN A SUBTYPE " &
      "DECLARATION OR IN A CONSTANT DECLARATION");

   declare
      type Rec (Disc : Integer) is record
         null;
      end record;

      subtype St is Rec;                               -- 1.

      C1 : constant Rec := (Disc => 5);                -- 2.
      C2 : constant Rec := (Disc => Ident_Int (5));     -- 2.
   begin

      if C1 /= C2 or C1 /= (Disc => 5) then
         Failed ("CONSTANT DECLARATIONS INCORRECT");
      end if;
   end;

   Result;
end C37206a;

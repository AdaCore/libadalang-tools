-- C48006A.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T'(X)" ALLOCATES A NEW
-- OBJECT EACH TIME IT IS EXECUTED AND THAT IF T IS A SCALAR OR ACCESS
-- TYPE, THE ALLOCATED OBJECT HAS THE VALUE OF X.

-- RM  01/14/80
-- RM  01/O1/82
-- SPS 10/27/82
-- EG  07/05/84

with Report;

procedure C48006a is

   use Report;

begin

   Test
     ("C48006A",
      "CHECK THAT THE FORM 'NEW T'(X)' " &
      "ALLOCATES A NEW OBJECT " &
      "AND THAT IF T IS A SCALAR OR ACCESS TYPE, THE " &
      "ALLOCATED OBJECT HAS THE VALUE OF X");

   declare

      type Ata is access Integer;
      type Aata is access Ata;
      Va1, Va2, Va3    : Ata;
      Vaa1, Vaa2, Vaa3 : Aata;

   begin

      Va1 := new Integer'(5 + 7);
      if Va1.all /= Ident_Int (12) then
         Failed ("WRONG VALUES - VA1");
      end if;

      Va2 := new Integer'(1 + 2);
      if (Va1.all /= Ident_Int (12) or Va2.all /= Ident_Int (3)) then
         Failed ("WRONG VALUES - VA2");
      end if;

      Va3 := new Integer'(Ident_Int (3) + Ident_Int (4));
      if
        (Va1.all /= Ident_Int (12) or
         Va2.all /= Ident_Int (3) or
         Va3.all /= Ident_Int (7))
      then
         Failed ("WRONG VALUES - VA3");
      end if;

      Vaa1 := new Ata'(new Integer'(3));
      if Vaa1.all.all /= Ident_Int (3) then
         Failed ("WRONG VALUES - VAA1");
      end if;

      Vaa2 := new Ata'(new Integer'(Ident_Int (5)));
      if (Vaa1.all.all /= 3 or Vaa2.all.all /= 5) then
         Failed ("WRONG VALUES - VAA2");
      end if;

      Vaa3 := new Ata'(new Integer'(Ident_Int (6)));
      if (Vaa1.all.all /= 3 or Vaa2.all.all /= 5 or Vaa3.all.all /= 6) then
         Failed ("WRONG VALUES - VAA3");
      end if;

   end;

   Result;

end C48006a;

-- C56002A.ADA

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
-- CHECK THAT BLOCKS CAN HAVE DECLARATIVE PARTS AND THAT
--    THE EFFECT OF THESE DECLARATIONS IS LIMITED TO THE BLOCKS
--    IN WHICH THEY OCCUR.

-- RM 04/16/81
-- SPS 3/4/83

with Report;
procedure C56002a is

   use Report;

begin

   Test
     ("C56002A",
      "BLOCKS CAN HAVE DECLARATIVE PARTS AND" &
      " THE EFFECT OF THESE DECLARATIONS IS LIMITED" &
      " TO THE BLOCKS IN WHICH THEY OCCUR");

   declare

      First  : constant Integer := Ident_Int (1);
      Second : constant Integer := Ident_Int (2);
      Third  : constant Integer := Ident_Int (3);
      Fourth : constant Integer := Ident_Int (4);
      Fifth  : constant Integer := Ident_Int (5);
      Tenth  : constant Integer := Ident_Int (10);
      Zeroth : constant Integer := Ident_Int (0);

   begin

      if First /= 1 or Second /= 2 or Third /= 3 or Fourth /= 4 or
        Fifth /= 5 or Tenth /= 10 or Zeroth /= 0 then
         Failed ("WRONG VALUES  -  1");
      end if;

      declare

         type Enum is (Aminus, A, B, C, D, E, F, G, H, I, J);

         First  : constant Enum := A;
         Second : constant Enum := B;
         Third  : constant Enum := C;
         Fourth : constant Enum := D;
         Fifth  : constant Enum := E;
         Tenth  : constant Enum := J;
         Zeroth : constant Enum := Aminus;

      begin

         if First /= Enum'Val (Ident_Int (1)) or
           Second /= Enum'Val (Ident_Int (2)) or
           Third /= Enum'Val (Ident_Int (3)) or
           Fourth /= Enum'Val (Ident_Int (4)) or
           Fifth /= Enum'Val (Ident_Int (5)) or
           Tenth /= Enum'Val (Ident_Int (10)) or
           Zeroth /= Enum'Val (Ident_Int (0)) then
            Failed ("WRONG VALUES  -  2");
         end if;

      end;

      if First /= 1 or Second /= 2 or Third /= 3 or Fourth /= 4 or
        Fifth /= 5 or Tenth /= 10 or Zeroth /= 0 then
         Failed ("WRONG VALUES  -  3");
      end if;

      declare

         First  : constant Character := 'A';
         Second : constant Character := 'B';
         Third  : constant Character := 'C';
         Fourth : constant Character := 'D';
         Fifth  : constant Character := 'E';
         Tenth  : constant Character := 'J';
         Zeroth : constant Character := '0';--ZERO < ANY LETTER

      begin

         if First /= Ident_Char ('A') or Second /= Ident_Char ('B') or
           Third /= Ident_Char ('C') or Fourth /= Ident_Char ('D') or
           Fifth /= Ident_Char ('E') or Tenth /= Ident_Char ('J') or
           Zeroth /= Ident_Char ('0') then
            Failed ("WRONG VALUES  -  4");
         end if;

      end;

      if First /= 1 or Second /= 2 or Third /= 3 or Fourth /= 4 or
        Fifth /= 5 or Tenth /= 10 or Zeroth /= 0 then
         Failed ("WRONG VALUES  -  5");
      end if;

   end;

   Result;

end C56002a;

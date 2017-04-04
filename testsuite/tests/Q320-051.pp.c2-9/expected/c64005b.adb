-- C64005B.ADA

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
-- CHECK THAT A SUBPROGRAM CAN BE CALLED RECURSIVELY AND THAT NON-LOCAL
-- VARIABLES AND CONSTANTS ARE PROPERLY ACCESSED FROM WITHIN RECURSIVE
-- INVOCATIONS.

-- CPP 7/2/84

with Report; use Report;
procedure C64005b is

   Count      : Integer          := 0;
   Twenty     : constant Integer := 20;
   C1         : constant Integer := 1;
   G1, G2, G3 : Integer          := 0;
   G4, G5     : Integer          := 0;

   procedure R (A1 : Integer; A2 : in out Integer; A3 : out Integer) is
      C1     : constant Integer := 5;
      Ten    : constant Integer := 10;
      J1, J2 : Integer          := 1;
      J3     : Integer          := 0;

      procedure Recurse (P1 : Integer; P2 : in out Integer) is
         C1 : Integer := 2;
      begin     -- RECURSE
         C1 := Ident_Int (10);
         if P1 < Twenty then
            Recurse (P1 + C1, G2);
            G1 := G1 + C64005b.C1;
            G3 := G3 + P1;
            P2 := P2 + Ident_Int (2);
            A2 := A2 + Ident_Int (1);
            J2 := J2 + R.C1;
         end if;
      end Recurse;

   begin     -- R
      if A2 < Ten then
         A2 := A2 + C1;
         Recurse (0, J1);
         J3    := J3 + Ten;
         Count := Count + 1;
         Comment ("ON PASS # " & Integer'Image (Count));
         Comment ("VALUE OF A2 IS " & Integer'Image (A2));
         Comment ("VALUE OF J3 IS " & Integer'Image (J3));
         R (0, A2, J3);
         J3 := J3 + A2;
      end if;
      A3 := J1 + J3;
   end R;

begin
   Test
     ("C64005B",
      "RECURSIVE SUBPROGRAMS WITH ALL KINDS " & "OF DATA ACCESS");

   R (0, G4, G5);

   if (Count /= 2) or
     (G1 /= 4) or
     (G2 /= 4) or
     (G3 /= 20) or
     (G4 /= 14) or
     (G5 /= 35)
   then
      Failed
        ("RECURSIVE INVOCATIONS' DATA ACCESS IS NOT" & " WORKING CORRECTLY");
   end if;

   Comment ("VALUE OF COUNT IS " & Integer'Image (Count));
   Comment ("VALUE OF G1 IS " & Integer'Image (G1));
   Comment ("VALUE OF G2 IS " & Integer'Image (G2));
   Comment ("VALUE OF G3 IS " & Integer'Image (G3));
   Comment ("VALUE OF G4 IS " & Integer'Image (G4));
   Comment ("VALUE OF G5 IS " & Integer'Image (G5));

   Result;

exception
   when Program_Error =>
      Failed ("PROGRAM_ERROR RAISED");
      Comment ("VALUE OF COUNT IS " & Integer'Image (Count));
      Comment ("VALUE OF G1 IS " & Integer'Image (G1));
      Comment ("VALUE OF G2 IS " & Integer'Image (G2));
      Comment ("VALUE OF G3 IS " & Integer'Image (G3));
      Comment ("VALUE OF G4 IS " & Integer'Image (G4));
      Comment ("VALUE OF G5 IS " & Integer'Image (G5));
      Result;

end C64005b;

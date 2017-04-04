-- C64005DC.ADA

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
-- JRK 7/30/84

separate (C64005d0m.C64005da.C64005db)

procedure C64005dc (L : Level; C : Call; T : in out Trace) is

   V : String (1 .. 2);

   M : constant Natural := Level'Pos (L) - Level'Pos (Level'First) + 1;
   N : constant Natural := 2 * M + 1;

begin

   V (1) := Ident_Char (Ascii.Lc_C);
   V (2) := C;

   -- APPEND ALL V TO T.
   T.S (T.E + 1 .. T.E + N) :=
     C64005d0m.V & C64005da.V & C64005db.V & C64005dc.V;
   T.E := T.E + N;

   case C is

      when '1' =>
         C64005da (Ident_Char (Level'First), Ident_Char ('2'), T);

      when '2' =>
         C64005dc (L, Ident_Char ('3'), T);

      when '3' =>
         -- APPEND MID-POINT SYMBOL TO T.
         T.S (T.E + 1) := Ident_Char ('=');
         T.E           := T.E + 1;

         -- G := CATENATE ALL V, L, C;
         G :=
           C64005d0m.V &
           C64005d0m.L &
           C64005da.V &
           C64005da.L &
           C64005da.C &
           C64005db.V &
           C64005db.L &
           C64005db.C &
           C64005dc.V &
           C64005dc.L &
           C64005dc.C;
   end case;

   -- APPEND ALL L AND C TO T IN REVERSE ORDER.
   T.S (T.E + 1 .. T.E + N) :=
     C64005dc.L &
     C64005dc.C &
     C64005db.L &
     C64005db.C &
     C64005da.L &
     C64005da.C &
     C64005d0m.L;
   T.E := T.E + N;

end C64005dc;

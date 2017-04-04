-- C64005D0M.ADA

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
-- CHECK THAT NESTED SUBPROGRAMS CAN BE CALLED RECURSIVELY AND THAT NON-LOCAL
-- VARIABLES AND FORMAL PARAMETERS ARE PROPERLY ACCESSED FROM WITHIN RECURSIVE
-- INVOCATIONS. THIS TEST CHECKS THAT EVERY DISPLAY OR STATIC CHAIN LEVEL CAN
-- BE ACCESSED.

-- THIS TEST USES 3 LEVELS OF NESTED RECURSIVE PROCEDURES (SEPARATELY COMPILED
-- AS SUBUNITS).

-- SEPARATE FILES ARE:
--   C64005D0M THE MAIN PROCEDURE.
--   C64005DA  A RECURSIVE PROCEDURE SUBUNIT OF C64005D0M.
--   C64005DB  A RECURSIVE PROCEDURE SUBUNIT OF C64005DA.
--   C64005DC  A RECURSIVE PROCEDURE SUBUNIT OF C64005DB.

-- JRK 7/30/84

with Report; use Report;

procedure C64005d0m is

   subtype Level is Character range 'A' .. 'C';
   subtype Call is Character range '1' .. '3';

   Max_Lev : constant := Level'Pos (Level'Last) - Level'Pos (Level'First) + 1;
   T_Len   : constant :=
     2 * (1 + 3 * (Max_Lev + Max_Lev * (Max_Lev + 1) / 2 * 2)) + 1;
   G_Len : constant := 2 + 4 * Max_Lev;

   type Trace is record
      E : Natural := 0;
      S : String (1 .. T_Len);
   end record;

   V : Character := Ident_Char ('<');
   L : Character := Ident_Char ('>');
   T : Trace;
   G : String (1 .. G_Len);

   procedure C64005da (L : Level; C : Call; T : in out Trace) is separate;

begin
   Test
     ("C64005D",
      "CHECK THAT NON-LOCAL VARIABLES AND FORMAL " &
      "PARAMETERS AT ALL LEVELS OF NESTED " &
      "RECURSIVE PROCEDURES ARE ACCESSIBLE (FOR " &
      "3 LEVELS OF SEPARATELY COMPILED SUBUNITS)");

   -- APPEND V TO T.
   T.S (T.E + 1) := V;
   T.E           := T.E + 1;

   C64005da (Ident_Char (Level'First), Ident_Char ('1'), T);

   -- APPEND L TO T.
   T.S (T.E + 1) := L;
   T.E           := T.E + 1;

   Comment ("FINAL CALL TRACE LENGTH IS: " & Integer'Image (T.E));
   Comment ("FINAL CALL TRACE IS: " & T.S (1 .. T.E));
   Comment ("GLOBAL SNAPSHOT IS: " & G);

   -- CHECK THAT T AND G ARE CORRECT BY COMPUTING THEM ITERATIVELY.

   declare
      subtype Lc_Level is
        Character range
          Ascii.Lc_A ..
            Character'Val (Character'Pos (Ascii.Lc_A) + Max_Lev - 1);

      Ct : Trace;
      Cg : String (1 .. G_Len);
   begin
      Comment ("CORRECT FINAL CALL TRACE LENGTH IS: " & Integer'Image (T_Len));

      if T.E /= Ident_Int (T_Len) then
         Failed ("WRONG FINAL CALL TRACE LENGTH");

      else
         Ct.S (Ct.E + 1) := '<';
         Ct.E            := Ct.E + 1;

         for I in Lc_Level loop
            Ct.S (Ct.E + 1) := '<';
            Ct.E            := Ct.E + 1;

            for J in Lc_Level'First .. I loop
               Ct.S (Ct.E + 1) := J;
               Ct.S (Ct.E + 2) := '1';
               Ct.E            := Ct.E + 2;
            end loop;
         end loop;

         for I in Lc_Level loop
            Ct.S (Ct.E + 1) := '<';
            Ct.E            := Ct.E + 1;

            for J in Lc_Level'First .. Lc_Level'Pred (I) loop
               Ct.S (Ct.E + 1) := J;
               Ct.S (Ct.E + 2) := '3';
               Ct.E            := Ct.E + 2;
            end loop;

            Ct.S (Ct.E + 1) := I;
            Ct.S (Ct.E + 2) := '2';
            Ct.E            := Ct.E + 2;

            Ct.S (Ct.E + 1) := '<';
            Ct.E            := Ct.E + 1;

            for J in Lc_Level'First .. I loop
               Ct.S (Ct.E + 1) := J;
               Ct.S (Ct.E + 2) := '3';
               Ct.E            := Ct.E + 2;
            end loop;
         end loop;

         Ct.S (Ct.E + 1) := '=';
         Ct.E            := Ct.E + 1;

         for I in reverse Level loop
            for J in reverse Level'First .. I loop
               Ct.S (Ct.E + 1) := J;
               Ct.S (Ct.E + 2) := '3';
               Ct.E            := Ct.E + 2;
            end loop;

            Ct.S (Ct.E + 1) := '>';
            Ct.E            := Ct.E + 1;

            Ct.S (Ct.E + 1) := I;
            Ct.S (Ct.E + 2) := '2';
            Ct.E            := Ct.E + 2;

            for J in reverse Level'First .. Level'Pred (I) loop
               Ct.S (Ct.E + 1) := J;
               Ct.S (Ct.E + 2) := '3';
               Ct.E            := Ct.E + 2;
            end loop;

            Ct.S (Ct.E + 1) := '>';
            Ct.E            := Ct.E + 1;
         end loop;

         for I in reverse Level loop
            for J in reverse Level'First .. I loop
               Ct.S (Ct.E + 1) := J;
               Ct.S (Ct.E + 2) := '1';
               Ct.E            := Ct.E + 2;
            end loop;

            Ct.S (Ct.E + 1) := '>';
            Ct.E            := Ct.E + 1;
         end loop;

         Ct.S (Ct.E + 1) := '>';
         Ct.E            := Ct.E + 1;

         if Ct.E /= Ident_Int (T_Len) then
            Failed ("WRONG ITERATIVE TRACE LENGTH");

         else
            Comment ("CORRECT FINAL CALL TRACE IS: " & Ct.S);

            if T.S /= Ct.S then
               Failed ("WRONG FINAL CALL TRACE");
            end if;
         end if;
      end if;

      declare
         E : Natural := 0;
      begin
         Cg (1 .. 2) := "<>";
         E           := E + 2;

         for I in Level loop
            Cg (E + 1) :=
              Lc_Level'Val
                (Level'Pos (I) -
                 Level'Pos (Level'First) +
                 Lc_Level'Pos (Lc_Level'First));
            Cg (E + 2) := '3';
            Cg (E + 3) := I;
            Cg (E + 4) := '3';
            E          := E + 4;
         end loop;

         Comment ("CORRECT GLOBAL SNAPSHOT IS: " & Cg);

         if G /= Cg then
            Failed ("WRONG GLOBAL SNAPSHOT");
         end if;
      end;
   end;

   Result;
end C64005d0m;

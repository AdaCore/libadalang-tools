-- C32001D.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR ACCESS TYPES, THE SUBTYPE
-- INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED ONCE FOR EACH
-- NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE INDICATION IS EVALUATED FIRST.
-- ALSO, CHECK THAT THE EVALUATIONS YIELD THE SAME RESULT AS A SEQUENCE OF
-- SINGLE OBJECT DECLARATIONS.

-- RJW 7/16/86

with Report; use Report;

procedure C32001d is

   type Arr is array (1 .. 2) of Integer;
   Bump : Arr := (0, 0);
   F1   : Arr;

   function F (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      F1 (I)   := Bump (I);
      return Bump (I);
   end F;

   function G (I : Integer) return Integer is
   begin
      Bump (I) := Bump (I) + 1;
      return Bump (I);
   end G;

begin
   Test
     ("C32001D",
      "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " &
      "FOR ACCESS TYPES, THE SUBTYPE INDICATION " &
      "AND THE INITIALIZATION EXPRESSIONS ARE " &
      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
      "EVALUATIONS YIELD THE SAME RESULT AS A " &
      "SEQUENCE OF SINGLE OBJECT DECLARATIONS");

   declare

      type Cell (Size : Integer) is record
         Value : Integer;
      end record;

      type Link is access Cell;

      L1, L2 : Link (F (1)) := new Cell'(F1 (1), G (1));

      Cl1, Cl2 : constant Link (F (2)) := new Cell'(F1 (2), G (2));

      procedure Check (L : Link; V1, V2 : Integer; S : String) is
      begin
         if L.Size /= V1 then
            Failed
              (S &
               ".SIZE INITIALIZED INCORRECTLY TO " &
               Integer'Image (L.Size));
         end if;

         if L.Value /= V2 then
            Failed
              (S &
               ".VALUE INITIALIZED INCORRECTLY TO " &
               Integer'Image (L.Value));
         end if;
      end Check;

   begin
      Check (L1, 1, 2, "L1");
      Check (L2, 3, 4, "L2");

      Check (Cl1, 1, 2, "CL1");
      Check (Cl2, 3, 4, "CL2");
   end;

   Result;
end C32001d;

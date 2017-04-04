-- C37310A.ADA

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
-- CHECK THAT IF A DISCRIMINANT HAS A DYNAMIC SUBTYPE, AN OTHERS CHOICE CAN BE
-- OMITTED IF ALL VALUES IN THE BASE TYPE'S RANGE ARE COVERED.

-- ASL 7/10/81
-- SPS 10/25/82
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report;
procedure C37310a is

   use Report;

begin
   Test
     ("C37310A",
      "CHECK DYNAMIC DISCRIMINANT SUBTYPES " &
      "IN VARIANT RECORD DECLARATIONS");

   declare

      Achar : Character := Ident_Char ('A');
      Echar : Character := Ident_Char ('E');
      Jchar : Character := Ident_Char ('J');
      Mchar : Character := Ident_Char ('M');
      subtype Statchar is Character range 'I' .. 'N';
      subtype Dynchar is Character range Achar .. Echar;
      subtype Sstat is Statchar range Jchar .. Mchar;

      type Letter is new Character range 'A' .. 'Z';
      subtype Dynletter is Letter range Letter (Echar) .. Letter (Jchar);

      type Rec1 (Disc : Sstat := 'K') is record
         case Disc is
            when Ascii.Nul .. Character'Last =>
               null;
         end case;
      end record;

      type Rec2 (Disc : Dynchar := 'C') is record
         case Disc is
            when Ascii.Nul .. Character'Last =>
               null;
         end case;
      end record;

      type Rec3 (Disc : Dynchar := 'D') is record
         case Disc is
            when Character'First .. Character'Last =>
               null;
         end case;
      end record;

      type Rec4 (Disc : Dynletter := 'F') is record
         case Disc is
            when Letter'Base'First .. Letter'Base'Last =>
               null;
         end case;
      end record;

      R1 : Rec1;
      R2 : Rec2;
      R3 : Rec3;
      R4 : Rec4;
   begin
      if Equal (3, 3) then
         R1 := (Disc => 'L');
      end if;
      if R1.Disc /= 'L' then
         Failed ("ASSIGNMENT FAILED - 1");
      end if;

      if Equal (3, 3) then
         R2 := (Disc => 'B');
      end if;
      if R2.Disc /= 'B' then
         Failed ("ASSIGNMENT FAILED - 2");
      end if;

      if Equal (3, 3) then
         R3 := (Disc => 'B');
      end if;
      if R3.Disc /= 'B' then
         Failed ("ASSIGNMENT FAILED - 3");
      end if;

      if Equal (3, 3) then
         R4 := (Disc => 'H');
      end if;
      if R4.Disc /= 'H' then
         Failed ("ASSIGNMENT FAILED - 4");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;

end C37310a;

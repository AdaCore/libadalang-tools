-- CD3015G.ADA

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
--     CHECK THAT A DERIVED ENUMERATION TYPE WITH A REPRESENTATION
--     CLAUSE CAN BE USED CORRECTLY IN ORDERING RELATIONS, INDEXING
--     ARRAYS, AND IN GENERIC INSTANTIATIONS WHEN THERE IS AN
--     ENUMERATION CLAUSE FOR THE PARENT.

-- HISTORY
--     DHH 09/30/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.
--     BCB 03/08/90  REVISED WORDING IN HEADER COMMENT AND IN CALL TO
--                   REPORT.TEST.  ADDED CHECK FOR NON-CONTIGUOUS CODES.
--                   REVISED CHECK FOR ARRAY INDEXING.
--     THS 09/18/90  REVISED WORDING IN HEADER COMMENT AND FIXED FAILURE
--                   ERROR MESSAGE.

with Report; use Report;
procedure Cd3015g is

begin

   Test
     ("CD3015G",
      "CHECK THAT A DERIVED ENUMERATION TYPE WITH A " &
      "REPRESENTATION CLAUSE CAN BE USED CORRECTLY " &
      "IN ORDERING RELATIONS, INDEXING ARRAYS, AND " &
      "IN GENERIC INSTANTIATIONS WHEN THERE IS AN " &
      "ENUMERATION CLAUSE FOR THE PARENT");

   declare
      package Pack is

         type Main is (Red, Blue, Yellow, 'R', 'B', 'Y');

         for Main use
           (Red    => 1,
            Blue   => 2,
            Yellow => 3,
            'R'    => 4,
            'B'    => 5,
            'Y'    => 6);

         type Hue is new Main;
         for Hue use
           (Red    => 8,
            Blue   => 9,
            Yellow => 10,
            'R'    => 11,
            'B'    => 12,
            'Y'    => 13);

         type Hue1 is new Main;
         for Hue1 use
           (Red    => 10,
            Blue   => 14,
            Yellow => 16,
            'R'    => 19,
            'B'    => 41,
            'Y'    => 46);

         type Base1 is array (Hue1) of Integer;
         Color1, Basic1 : Hue1;
         Barray1        : Base1;

         type Base is array (Hue) of Integer;
         Color, Basic : Hue;
         Barray       : Base;

         generic
            type Enum is (<>);
         procedure Change (X, Y : in out Enum);

      end Pack;

      package body Pack is

         procedure Change (X, Y : in out Enum) is
            T : Enum;
         begin
            T := X;
            X := Y;
            Y := T;
         end Change;

         procedure Proc is new Change (Hue);
         procedure Proc1 is new Change (Hue1);

      begin
         Basic  := Red;
         Color  := Hue'Succ (Basic);
         Basic1 := Red;
         Color1 := Hue1'Succ (Basic1);
         if (Color < Basic or Basic >= 'R' or 'Y' <= Color or Color > 'B') or
           not
           (Color1 >= Basic1 and
            Basic1 < 'R' and
            'Y' > Color1 and
            Color1 <= 'B')
         then
            Failed ("ORDERING RELATIONS ARE INCORRECT");
         end if;

         Proc (Basic, Color);
         Proc1 (Basic1, Color1);

         if Color /= Red or Color1 /= Red then
            Failed
              ("VALUES OF PARAMETERS TO INSTANCE OF " &
               "GENERIC UNIT NOT CORRECT AFTER CALL");
         end if;

         Barray :=
           (Ident_Int (1),
            Ident_Int (2),
            Ident_Int (3),
            Ident_Int (4),
            Ident_Int (5),
            Ident_Int (6));

         Barray1 :=
           (Ident_Int (1),
            Ident_Int (2),
            Ident_Int (3),
            Ident_Int (4),
            Ident_Int (5),
            Ident_Int (6));

         if
           (Barray (Red) /= 1 or
            Barray (Blue) /= 2 or
            Barray (Yellow) /= 3 or
            Barray ('R') /= 4 or
            Barray ('B') /= 5 or
            Barray ('Y') /= 6) or
           not
           (Barray1 (Red) = 1 and
            Barray1 (Blue) = 2 and
            Barray1 (Yellow) = 3 and
            Barray1 ('R') = 4 and
            Barray1 ('B') = 5 and
            Barray1 ('Y') = 6)
         then
            Failed ("INDEXING ARRAY FAILURE");
         end if;

      end Pack;
   begin
      null;
   end;

   Result;
end Cd3015g;

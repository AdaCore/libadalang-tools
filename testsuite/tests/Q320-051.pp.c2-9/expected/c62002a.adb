-- C62002A.ADA

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
-- CHECK THAT THE COMPONENTS OF ACCESS IN PARAMETERS CAN BE USED AS THE
--   TARGET OF AN ASSIGNMENT STATEMENT OR AS AN ACTUAL PARAMETER OF
--   ANY MODE.  SUBTESTS ARE:
--        (A) INTEGER ACCESS TYPE.
--        (B) ARRAY ACCESS TYPE.
--        (C) RECORD ACCESS TYPE.

-- DAS  1/23/81
-- SPS 10/26/82

with Report;
procedure C62002a is

   use Report;

begin

   Test
     ("C62002A",
      "CHECK THAT COMPONENTS OF ACCESS IN PARAMETERS" &
      " MAY BE USED IN ASSIGNMENT CONTEXTS");

   --------------------------------------------------

   declare   -- (A)

      type Ptrint is access Integer;
      Pi : Ptrint;

      procedure Proca (Pi : in Ptrint) is

         procedure Proca1 (I : out Integer) is
         begin
            I := 7;
         end Proca1;

         procedure Proca2 (I : in out Integer) is
         begin
            I := I + 1;
         end Proca2;
      begin

         Proca1 (Pi.all);
         Proca2 (Pi.all);
         Pi.all := Pi.all + 1;
         if (Pi.all /= 9) then
            Failed
              ("ASSIGNMENT TO COMPONENT OF INTEGER" &
               " ACCESS PARAMETER FAILED");
         end if;
      end Proca;

   begin     -- (A)

      Pi := new Integer'(0);
      Proca (Pi);

   end;      -- (A)

   ---------------------------------------------

   declare   -- (B)

      type Tbl is array (1 .. 3) of Integer;
      type Ptrtbl is access Tbl;
      Pt : Ptrtbl;

      procedure Procb (Pt : in Ptrtbl) is

         procedure Procb1 (I : out Integer) is
         begin
            I := 7;
         end Procb1;

         procedure Procb2 (I : in out Integer) is
         begin
            I := I + 1;
         end Procb2;

         procedure Procb3 (T : out Tbl) is
         begin
            T := (1, 2, 3);
         end Procb3;

         procedure Procb4 (T : in out Tbl) is
         begin
            T (3) := T (3) - 1;
         end Procb4;

      begin

         Procb3 (Pt.all);         -- (1,2,3)
         Procb4 (Pt.all);         -- (1,2,2)
         Procb1 (Pt (2));          -- (1,7,2)
         Procb2 (Pt (1));          -- (2,7,2)
         Pt (3) := Pt (3) + 7;      -- (2,7,9)
         if (Pt.all /= (2, 7, 9)) then
            Failed
              ("ASSIGNMENT TO COMPONENT OF ARRAY" &
               " ACCESS PARAMETER FAILED");
         end if;
      end Procb;

   begin     -- (B)

      Pt := new Tbl'(0, 0, 0);
      Procb (Pt);

   end;      -- (B)

   ---------------------------------------------

   declare   -- (C)

      type Rec is record
         I1 : Integer;
         I2 : Integer;
         I3 : Integer;
      end record;
      type Ptrrec is access Rec;
      Pr : Ptrrec;

      procedure Procc (Pr : in Ptrrec) is

         procedure Procc1 (I : out Integer) is
         begin
            I := 7;
         end Procc1;

         procedure Procc2 (I : in out Integer) is
         begin
            I := I + 1;
         end Procc2;

         procedure Procc3 (R : out Rec) is
         begin
            R := (1, 2, 3);
         end Procc3;

         procedure Procc4 (R : in out Rec) is
         begin
            R.I3 := R.I3 - 1;
         end Procc4;

      begin

         Procc3 (Pr.all);         -- (1,2,3)
         Procc4 (Pr.all);         -- (1,2,2)
         Procc1 (Pr.I2);          -- (1,7,2)
         Procc2 (Pr.I1);          -- (2,7,2)
         Pr.I3 := Pr.I3 + 7;      -- (2,7,9)
         if (Pr.all /= (2, 7, 9)) then
            Failed
              ("ASSIGNMENT TO COMPONENT OF RECORD" &
               " ACCESS PARAMETER FAILED");
         end if;
      end Procc;

   begin     -- (C)

      Pr := new Rec'(0, 0, 0);
      Procc (Pr);

   end;      -- (C)

   ---------------------------------------------

   Result;

end C62002a;

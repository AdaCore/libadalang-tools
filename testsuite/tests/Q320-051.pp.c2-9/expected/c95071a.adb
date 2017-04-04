-- C95071A.ADA

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
-- CHECK THAT OBJECTS DESIGNATED BY IN PARAMETERS OF ACCESS TYPES CAN BE USED
-- AS THE TARGET OF AN ASSIGNMENT STATEMENT AND AS AN ACTUAL PARAMETER OF ANY
-- MODE. SUBTESTS ARE:
--        (A) INTEGER ACCESS TYPE.
--        (B) ARRAY ACCESS TYPE.
--        (C) RECORD ACCESS TYPE.

-- JWC 7/11/85

with Report; use Report;
procedure C95071a is

begin

   Test
     ("C95071A",
      "CHECK THAT COMPONENTS OF ACCESS IN PARAMETERS " &
      "MAY BE USED IN ASSIGNMENT CONTEXTS");

   --------------------------------------------------

   declare   -- (A)

      type Ptrint is access Integer;
      Pi : Ptrint;

      task Ta is
         entry Ea (Pi : in Ptrint);
      end Ta;

      task body Ta is
      begin
         accept Ea (Pi : in Ptrint) do
            declare
               task Ta1 is
                  entry Ea1 (I : out Integer);
                  entry Ea2 (I : in out Integer);
               end Ta1;

               task body Ta1 is
               begin
                  accept Ea1 (I : out Integer) do
                     I := 7;
                  end Ea1;

                  accept Ea2 (I : in out Integer) do
                     I := I + 1;
                  end Ea2;
               end Ta1;

            begin
               Ta1.Ea1 (Pi.all);
               Ta1.Ea2 (Pi.all);
               Pi.all := Pi.all + 1;
               if (Pi.all /= 9) then
                  Failed
                    ("ASSIGNMENT TO COMPONENT OF " &
                     "INTEGER ACCESS PARAMETER " &
                     "FAILED");
               end if;
            end;
         end Ea;
      end Ta;

   begin     -- (A)

      Pi := new Integer'(0);
      Ta.Ea (Pi);

   end;      -- (A)

   ---------------------------------------------

   declare   -- (B)

      type Tbl is array (1 .. 3) of Integer;
      type Ptrtbl is access Tbl;
      Pt : Ptrtbl;

      task Tb is
         entry Eb (Pt : in Ptrtbl);
      end Tb;

      task body Tb is
      begin
         accept Eb (Pt : in Ptrtbl) do
            declare
               task Tb1 is
                  entry Eb1 (T : out Tbl);
                  entry Eb2 (T : in out Tbl);
                  entry Eb3 (I : out Integer);
                  entry Eb4 (I : in out Integer);
               end Tb1;

               task body Tb1 is
               begin
                  accept Eb1 (T : out Tbl) do
                     T := (1, 2, 3);
                  end Eb1;

                  accept Eb2 (T : in out Tbl) do
                     T (3) := T (3) - 1;
                  end Eb2;

                  accept Eb3 (I : out Integer) do
                     I := 7;
                  end Eb3;

                  accept Eb4 (I : in out Integer) do
                     I := I + 1;
                  end Eb4;
               end Tb1;

            begin
               Tb1.Eb1 (Pt.all);         -- (1,2,3)
               Tb1.Eb2 (Pt.all);         -- (1,2,2)
               Tb1.Eb3 (Pt (2));          -- (1,7,2)
               Tb1.Eb4 (Pt (1));          -- (2,7,2)
               Pt (3) := Pt (3) + 7;      -- (2,7,9)
               if (Pt.all /= (2, 7, 9)) then
                  Failed
                    ("ASSIGNMENT TO COMPONENT OF " &
                     "ARRAY ACCESS PARAMETER FAILED");
               end if;
            end;
         end Eb;
      end Tb;

   begin     -- (B)

      Pt := new Tbl'(0, 0, 0);
      Tb.Eb (Pt);

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

      task Tc is
         entry Ec (Pr : in Ptrrec);
      end Tc;

      task body Tc is
      begin
         accept Ec (Pr : in Ptrrec) do
            declare
               task Tc1 is
                  entry Ec1 (R : out Rec);
                  entry Ec2 (R : in out Rec);
                  entry Ec3 (I : out Integer);
                  entry Ec4 (I : in out Integer);
               end Tc1;

               task body Tc1 is
               begin
                  accept Ec1 (R : out Rec) do
                     R := (1, 2, 3);
                  end Ec1;

                  accept Ec2 (R : in out Rec) do
                     R.I3 := R.I3 - 1;
                  end Ec2;

                  accept Ec3 (I : out Integer) do
                     I := 7;
                  end Ec3;

                  accept Ec4 (I : in out Integer) do
                     I := I + 1;
                  end Ec4;
               end Tc1;

            begin
               Tc1.Ec1 (Pr.all);         -- (1,2,3)
               Tc1.Ec2 (Pr.all);         -- (1,2,2)
               Tc1.Ec3 (Pr.I2);          -- (1,7,2)
               Tc1.Ec4 (Pr.I1);          -- (2,7,2)
               Pr.I3 := Pr.I3 + 7;       -- (2,7,9)
               if (Pr.all /= (2, 7, 9)) then
                  Failed
                    ("ASSIGNMENT TO COMPONENT OF " &
                     "RECORD ACCESS PARAMETER " &
                     "FAILED");
               end if;
            end;
         end Ec;
      end Tc;

   begin     -- (C)

      Pr := new Rec'(0, 0, 0);
      Tc.Ec (Pr);

   end;      -- (C)

   ---------------------------------------------

   Result;

end C95071a;

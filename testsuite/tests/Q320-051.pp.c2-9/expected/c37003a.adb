-- C37003A.ADA

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
-- CHECK THAT MULTIPLE COMPONENT DECLARATIONS ARE TREATED AS A SERIES OF SINGLE
-- COMNENT DECLARATIONS, I.E., THE COMPONENTS ALL HAVE THE SAME TYPE AND ANY
-- EXPRESSION USED IN CONSTRAINTS OR INITIALIZATIONS IS EVALUATED ONCE FOR
-- EACH COMPONENT.

-- DAT 3/30/81
-- SPS 10/26/82
-- JWC 10/23/85 RENAMED FROM C37013A-AB.ADA.
--               ADDED TEST TO ENSURE THAT ANY EXPRESSION USED
--               IN A CONSTRAINT IS EVALUATED ONCE FOR EACH
--               COMPONENT.
-- JRK 11/15/85 ADDED INITIALIZATION EVALUATION CHECKS.

with Report; use Report;

procedure C37003a is

   X : Integer := 0;

   function F return Integer is
   begin
      X := X + 1;
      return X;
   end F;

   procedure Reset is
   begin
      X := 0;
   end Reset;

begin
   Test
     ("C37003A",
      "CHECK THAT MULTIPLE COMPONENT DECLARATIONS " &
      "ARE TREATED AS A SERIES OF SINGLE COMPONENT " &
      "DECLARATIONS");

   declare

      type Arr is array (Integer range <>) of Integer;

      type Rec1 is record
         A1, A2 : Arr (1 .. F) := (others => F);
      end record;

      R1  : Rec1    := (others => (others => 1));
      Y   : Integer := X;
      R1a : Rec1;

   begin

      if R1.A1 = R1.A2 then        -- TEST TO SEE IF THE COMPONENTS
         null;                   -- ARE OF THE SAME TYPE.
      end if;

      if Y /= 2 then
         Failed ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " & "FOR ARRAYS");
      end if;

      if X /= 5 then
         Failed
           ("INITIALIZATION EXPRESSION NOT EVALUATED FOR " &
            "EACH ARRAY COMPONENT");
      end if;

      Reset;

   end;

   declare

      type Rec2 is record
         I1, I2 : Integer range 1 .. F := F * Ident_Int (0) + 1;
      end record;

      R2  : Rec2    := (others => 1);
      Y   : Integer := X;
      R2a : Rec2;

   begin

      if R2.I1 = R2.I2 then        -- TEST TO SEE IF THE COMPONENTS
         null;                   -- ARE OF THE SAME TYPE.
      end if;

      if Y /= 2 then
         Failed ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " & "FOR SCALARS");
      end if;

      if X /= 4 then
         Failed
           ("INITIALIZATION EXPRESSION NOT EVALUATED FOR " &
            "EACH SCALAR COMPONENT");
      end if;

      Reset;

   end;

   declare

      type Rec3x (Dsc : Integer) is record
         null;
      end record;

      type Rec3y is record
         I : Integer;
      end record;

      type Rec3 is record
         Rx1, Rx2 : Rec3x (F);
         Ry1, Ry2 : Rec3y := (I => F);
      end record;

      R3  : Rec3    := ((Dsc => 1), (Dsc => 2), (I => 0), (I => 0));
      Y   : Integer := X;
      R3a : Rec3;

   begin

      if R3.Rx1 = R3.Rx2 then       -- TEST TO SEE IF THE COMPONENTS
         null;                    -- ARE OF THE SAME TYPE.
      end if;

      if Y /= 2 then
         Failed ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " & "FOR RECORDS");
      end if;

      if X /= 4 then
         Failed
           ("INITIALIZATION EXPRESSION NOT EVALUATED " &
            "FOR EACH RECORD COMPONENT");
      end if;

      Reset;

   end;

   declare

      type Rec4x (Dsc : Integer) is record
         null;
      end record;

      type Acr is access Rec4x;
      type Aci is access Integer;

      type Rec4 is record
         Ac1, Ac2 : Acr (F);
         Ac3, Ac4 : Aci := new Integer'(F);
      end record;

      R4  : Rec4    := (null, null, null, null);
      Y   : Integer := X;
      R4a : Rec4;

   begin

      if R4.Ac1 = R4.Ac2 then       -- TEST TO SEE IF THE COMPONENTS
         null;                    -- ARE OF THE SAME TYPE.
      end if;

      if Y /= 2 then
         Failed ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " & "FOR ACCESS");
      end if;

      if X /= 4 then
         Failed
           ("INITIALIZATION EXPRESSION NOT EVALUATED " &
            "FOR EACH ACCESS COMPONENT");
      end if;

   end;

   Result;
end C37003a;

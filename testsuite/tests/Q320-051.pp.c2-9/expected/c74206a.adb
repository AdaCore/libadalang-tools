-- C74206A.ADA

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
-- CHECK THAT IF A COMPOSITE TYPE IS DECLARED IN THE PACKAGE AS A
-- PRIVATE TYPE AND CONTAINS A COMPONENT OF THE PRIVATE TYPE, OPERATIONS
-- OF THE COMPOSITE TYPE WHICH DO NOT DEPEND ON CHARACTERISTICS OF THE
-- PRIVATE TYPE ARE AVAILABLE AFTER THE FULL DECLARATION OF THE PRIVATE
-- TYPE, BUT BEFORE THE EARLIEST PLACE WITHIN THE IMMEDIATE SCOPE OF THE
-- DECLARATION OF THE COMPOSITE TYPE THAT IS AFTER THE FULL DECLARATION
-- OF THE PRIVATE TYPE.  IN PARTICULAR, CHECK FOR THE FOLLOWING :

--   'FIRST, 'LAST, 'RANGE, AND 'LENGTH FOR ARRAY TYPES
--   SELECTED COMPONENTS FOR DISCRIMINANTS AND COMPONENTS OF RECORDS
--   INDEXED COMPONENTS AND SLICES FOR ARRAYS

-- DSJ 5/5/83
-- JBG 3/8/84

with Report;
procedure C74206a is

   use Report;

begin

   Test
     ("C74206A",
      "CHECK THAT ADDITIONAL OPERATIONS FOR " &
      "COMPOSITE TYPES OF PRIVATE TYPES ARE " &
      "AVAILABLE AT THE EARLIEST PLACE AFTER THE " &
      "FULL DECLARATION OF THE PRIVATE TYPE EVEN " &
      "IF BEFORE THE EARLIEST PLACE WITHIN THE " &
      "IMMEDIATE SCOPE OF THE COMPOSITE TYPE");

   declare

      package Pack1 is
         type P1 is private;
         type Lp1 is limited private;

         package Pack_Lp is
            type Lp_Arr is array (1 .. 2) of Lp1;
            type Lp_Rec (D : Integer) is record
               C1, C2 : Lp1;
            end record;
         end Pack_Lp;

         package Pack2 is
            type Arr is array (1 .. 2) of P1;
            type Rec (D : Integer) is record
               C1, C2 : P1;
            end record;
         end Pack2;
      private
         type P1 is new Boolean;
         type Lp1 is new Boolean;
      end Pack1;

      package body Pack1 is

         use Pack_Lp;
         use Pack2;

         A1 : Arr;
         L1 : Lp_Arr;

         N1 : Integer := Arr'First;           -- LEGAL
         N2 : Integer := Arr'Last;            -- LEGAL
         N3 : Integer := A1'Length;           -- LEGAL
         N4 : Integer := Lp_Arr'First;        -- LEGAL
         N5 : Integer := Lp_Arr'Last;         -- LEGAL
         N6 : Integer := L1'Length;           -- LEGAL
         B1 : Boolean := 1 in Arr'Range;      -- LEGAL
         B2 : Boolean := 5 in Lp_Arr'Range;   -- LEGAL

         N7 : Integer := A1 (1)'Size;          -- LEGAL: A1(1)
         N8 : Integer := L1 (2)'Size;          -- LEGAL: L1(2)

         R1 : Rec (1);
         Q1 : Lp_Rec (1);

         K1 : Integer := R1.D'Size;           -- LEGAL: R1.D
         K2 : Integer := R1.C1'Size;          -- LEGAL: R1.C1
         K3 : Integer := Q1.D'Size;           -- LEGAL: Q1.D
         K4 : Integer := Q1.C2'Size;          -- LEGAL: Q1.C2

      begin

         if N1 /= 1 or N4 /= 1 then
            Failed ("WRONG VALUE FOR 'FIRST");
         end if;

         if N2 /= 2 or N5 /= 2 then
            Failed ("WRONG VALUE FOR 'LAST");
         end if;

         if N3 /= 2 or N6 /= 2 then
            Failed ("WRONG VALUE FOR 'LENGTH");
         end if;

         if B1 /= True or B2 /= False then
            Failed ("INCORRECT RANGE TEST");
         end if;

         if N7 /= N8 then
            Failed ("INCORRECT INDEXED COMPONENTS");
         end if;

         if K1 /= K3 or K2 /= K4 then
            Failed ("INCORRECT COMPONENT SELECTION");
         end if;

      end Pack1;

   begin

      null;

   end;

   Result;

end C74206a;

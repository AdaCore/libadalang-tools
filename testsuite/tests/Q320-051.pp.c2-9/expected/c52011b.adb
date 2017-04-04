-- C52011B.ADA

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
-- CHECK DISCRIMINANT CONSTRAINTS FOR ASSIGNMENT OF ACCESS SUBTYPES.
-- SPECIFICALLY, CHECK THAT:

-- A) ANY ACCESS TYPE VARIABLE AND CONSTRAINED SUBTYPE VARIABLES OF THAT TYPE
-- MAY BE ASSIGNED TO ONE ANOTHER IF THE VALUE BEING ASSIGNED IS NULL.

-- B) VARIABLES OF THE SAME CONSTRAINED ACCESS SUBTYPE MAY BE ASSIGNED TO ONE
-- ANOTHER OR TO VARIABLES OF THE BASE ACCESS TYPE.

-- C) CONSTRAINT_ERROR IS RAISED UPON ASSIGNMENT OF NON-NULL OBJECTS BETWEEN
-- DIFFERENTLY CONSTRAINED ACCESS SUBTYPES.

-- D) CONSTRAINT_ERROR IS RAISED UPON ASSIGNMENT OF A NON-NULL OBJECT OF A BASE
-- ACCESS TYPE VARIABLE TO A VARIABLE OF ONE OF ITS CONSTRAINED SUBTYPES IF THE
-- CONSTRAINTS ON THE OBJECT DIFFER FROM THOSE ON THE SUBTYPE.

-- E) NULL CAN BE ASSIGNED TO BASE ACCESS TYPES AND ANY CONSTRAINED SUBTYPES OF
-- THIS TYPE.

-- ASL 7/06/81
--  RM 6/17/82
-- RLB 6/29/01 - FIXED TO ALLOW AGGRESIVE OPTIMIZATION.

with Report;
procedure C52011b is

   use Report;

   type Rec (Disc : Integer := -1) is record
      null;
   end record;

   type Rec_Name is access Rec;
   subtype S1 is Rec_Name (Ident_Int (5));
   subtype S2 is Rec_Name (Ident_Int (3));

   W      : Rec_Name := null;                    -- E.
   X1, X2 : S1       := null;                      -- E.
   Y1, Y2 : S2       := null;                      -- E.

   W_Nonnull  : Rec_Name := new Rec (7);
   X1_Nonnull : S1       := new Rec (Ident_Int (5));
   Y1_Nonnull : S2       := new Rec (Ident_Int (3));

   Too_Early : Boolean := True;

begin

   Test
     ("C52011B",
      "DISCRIMINANT CONSTRAINTS ON ACCESS SUBTYPE " &
      "OBJECTS MUST BE SATISFIED FOR ASSIGNMENT");

   begin

      if Equal (3, 3) then
         W_Nonnull := X1;               -- A.
      end if;
      if W_Nonnull /= X1 then
         Failed ("ASSIGNMENT FAILED - 1");
      end if;

      if Equal (3, 3) then
         W := Y1;                       -- A.
      end if;
      if W /= Y1 then
         Failed ("ASSIGNMENT FAILED - 2");
      end if;

      if Equal (3, 3) then
         X1_Nonnull := Y1;              -- A.
      end if;
      if X1_Nonnull /= Y1 then
         Failed ("ASSIGNMENT FAILED - 3");
      end if;

      if Equal (3, 3) then
         Y1_Nonnull := Y2;              -- A.
      end if;
      if Y1_Nonnull /= Y2 then
         Failed ("ASSIGNMENT FAILED - 4");
      end if;

      X1 := new Rec (Ident_Int (5));
      if Equal (3, 3) then
         X2 := X1;                      -- B.
      end if;
      if X1 /= X2 then
         Failed ("ASSIGNMENT FAILED - 5");
      end if;

      if Equal (3, 3) then
         W := X1;                       -- B.
      end if;
      if W /= X1 then
         Failed ("ASSIGNMENT FAILED - 6");
      end if;

      begin
         Y1 := X1;                      -- C.
         if Y1.Disc /= Report.Ident_Int (3) then
            Failed
              ("NON-NULL ASSIGNMENT MADE BETWEEN TWO " &
               "VARIABLES OF DIFFERENT CONSTRAINED ACCESS SUBTYPES " &
               "AND CONSTRAINT IS CHANGED");
         else
            Failed
              ("NON-NULL ASSIGNMENT MADE BETWEEN TWO " &
               "VARIABLES OF DIFFERENT CONSTRAINED ACCESS SUBTYPES " &
               "AND CONSTRAINT IS NOT CHANGED");
         end if;
      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("WRONG EXCEPTION - 1");

      end;

      W := new Rec (Ident_Int (3));

      begin
         X1 := W;                            -- D.
         if X1.Disc /= Report.Ident_Int (5) then
            Failed
              ("NON-NULL ASSIGNMENT MADE FROM UNCONSTRAINED " &
               "ACCESS TYPE DESIGNATING CONSTRAINED OBJECT TO " &
               "ACCESS SUBTYPE WITH DIFFERENT CONSTRAINT " &
               "AND CONSTRAINT IS CHANGED");
         else
            Failed
              ("NON-NULL ASSIGNMENT MADE FROM UNCONSTRAINED " &
               "ACCESS TYPE DESIGNATING CONSTRAINED OBJECT TO " &
               "ACCESS SUBTYPE WITH DIFFERENT CONSTRAINT " &
               "AND CONSTRAINT IS NOT CHANGED");
         end if;
      exception

         when Constraint_Error =>
            null;

         when others =>
            Failed ("WRONG EXCEPTION - 2");

      end;

   exception

      when others =>
         Failed ("EXCEPTION RAISED");

   end;

   Result;

end C52011b;

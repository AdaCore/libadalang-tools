-- CC1207B.ADA

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
--     CHECK THAT AN UNCONSTRAINED FORMAL TYPE WITH DISCRIMINANTS IS
--     ALLOWED AS THE TYPE OF A SUBPROGRAM OR AN ENTRY FORMAL
--     PARAMETER, AND AS THE TYPE OF A GENERIC FORMAL OBJECT PARAMETER,
--     AS A GENERIC ACTUAL PARAMETER, AND IN A MEMBERSHIP TEST, IN A
--     SUBTYPE DECLARATION, IN AN ACCESS TYPE DEFINITION, AND IN A
--     DERIVED TYPE DEFINITION.

-- HISTORY:
--     BCB 08/04/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure Cc1207b is

   generic
      type X (L : Integer) is private;
   package Pack is
   end Pack;

begin
   Test
     ("CC1207B",
      "CHECK THAT AN UNCONSTRAINED FORMAL TYPE WITH " &
      "DISCRIMINANTS IS ALLOWED AS THE TYPE OF A " &
      "SUBPROGRAM OR AN ENTRY FORMAL PARAMETER, AND " &
      "AS THE TYPE OF A GENERIC FORMAL OBJECT " &
      "PARAMETER, AS A GENERIC ACTUAL PARAMETER, AND " &
      "IN A MEMBERSHIP TEST, IN A SUBTYPE " &
      "DECLARATION, IN AN ACCESS TYPE DEFINITION, " &
      "AND IN A DERIVED TYPE DEFINITION");

   declare
      type Rec (D : Integer := 3) is record
         null;
      end record;

      generic
         type R (D : Integer) is private;
         Obj : R;
      package P is
         procedure S (X : R);

         task T is
            entry E (Y : R);
         end T;

         subtype Sub_R is R;

         type Acc_R is access R;

         type New_R is new R;

         Bool : Boolean := (Obj in R);

         Sub_Var : Sub_R (5);

         Acc_Var : Acc_R := new R (5);

         New_Var : New_R (5);

         package New_Pack is new Pack (R);
      end P;

      Rec_Var : Rec (5) := (D => 5);

      package body P is
         procedure S (X : R) is
         begin
            if not Equal (X.D, 5) then
               Failed ("WRONG DISCRIMINANT VALUE - S");
            end if;
         end S;

         task body T is
         begin
            accept E (Y : R) do
               if not Equal (Y.D, 5) then
                  Failed ("WRONG DISCRIMINANT VALUE - T");
               end if;
            end E;
         end T;
      begin
         if not Equal (Obj.D, 5) then
            Failed ("IMPROPER DISCRIMINANT VALUE");
         end if;

         S (Obj);

         T.E (Obj);

         if not Equal (Sub_Var.D, 5) then
            Failed ("IMPROPER DISCRIMINANT VALUE - SUBTYPE");
         end if;

         if not Equal (Acc_Var.D, 5) then
            Failed ("IMPROPER DISCRIMINANT VALUE - ACCESS");
         end if;

         if not Equal (New_Var.D, 5) then
            Failed ("IMPROPER DISCRIMINANT VALUE - DERIVED");
         end if;

         if not Bool then
            Failed ("IMPROPER RESULT FROM MEMBERSHIP TEST");
         end if;
      end P;

      package New_P is new P (Rec, Rec_Var);

   begin
      null;
   end;

   Result;
end Cc1207b;

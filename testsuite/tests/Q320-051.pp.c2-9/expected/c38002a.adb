-- C38002A.ADA

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
--     CHECK THAT AN UNCONSTRAINED ARRAY TYPE OR A RECORD WITHOUT
--     DEFAULT DISCRIMINANTS CAN BE USED IN AN ACCESS_TYPE_DEFINITION
--     WITHOUT AN INDEX OR DISCRIMINANT CONSTRAINT.
--
--     CHECK THAT (NON-STATIC) INDEX OR DISCRIMINANT CONSTRAINTS CAN
--     SUBSEQUENTLY BE IMPOSED WHEN THE TYPE IS USED IN AN OBJECT
--     DECLARATION, ARRAY COMPONENT DECLARATION, RECORD COMPONENT
--     DECLARATION, ACCESS TYPE DECLARATION, PARAMETER DECLARATION,
--     DERIVED TYPE DEFINITION, PRIVATE TYPE.
--
--     CHECK FOR UNCONSTRAINED GENERIC FORMAL TYPE.

-- HISTORY:
--     AH  09/02/86 CREATED ORIGINAL TEST.
--     DHH 08/16/88 REVISED HEADER AND ENTERED COMMENTS FOR PRIVATE TYPE
--                  AND CORRECTED INDENTATION.
--     BCB 04/12/90 ADDED CHECKS FOR AN ARRAY AS A SUBPROGRAM RETURN
--                  TYPE AND AN ARRAY AS A FORMAL PARAMETER.
--     LDC 10/01/90 ADDED CODE SO F, FPROC, G, GPROC AREN'T OPTIMIZED
--                  AWAY

with Report; use Report;
procedure C38002a is

begin
   Test
     ("C38002A",
      "NON-STATIC CONSTRAINTS CAN BE IMPOSED " &
      "ON ACCESS TYPES ACCESSING PREVIOUSLY UNCONSTRAINED " &
      "ARRAY OR RECORD TYPES");

   declare
      C3 : constant Integer := Ident_Int (3);

      type Arr is array (Integer range <>) of Integer;
      type Arr_Name is access Arr;
      subtype Arr_Name_3 is Arr_Name (1 .. 3);

      type Rec (Disc : Integer) is record
         Comp : Arr_Name (1 .. Disc);
      end record;
      type Rec_Name is access Rec;

      Obj : Rec_Name (C3);

      type Arr2 is array (1 .. 10) of Rec_Name (C3);

      type Rec2 is record
         Comp2 : Rec_Name (C3);
      end record;

      type Name_Rec_Name is access Rec_Name (C3);

      type Deriv is new Rec_Name (C3);
      subtype Rec_Name_3 is Rec_Name (C3);

      function F (Parm : Rec_Name_3) return Rec_Name_3 is
      begin
         if not Equal (Ident_Int (3), 1 + Ident_Int (2)) then
            Comment ("DON'T OPTIMIZE F AWAY");
         end if;
         return Parm;
      end F;

      procedure Fproc (Parm : Rec_Name_3) is
      begin
         if not Equal (Ident_Int (4), 2 + Ident_Int (2)) then
            Comment ("DON'T OPTIMIZE FPROC AWAY");
         end if;
      end Fproc;

      function G (Pa : Arr_Name_3) return Arr_Name_3 is
      begin
         if not Equal (Ident_Int (5), 3 + Ident_Int (2)) then
            Comment ("DON'T OPTIMIZE G AWAY");
         end if;
         return Pa;
      end G;

      procedure Gproc (Pa : Arr_Name_3) is
      begin
         if not Equal (Ident_Int (6), 4 + Ident_Int (2)) then
            Comment ("DON'T OPTIMIZE GPROC AWAY");
         end if;
      end Gproc;

   begin
      declare
         R : Rec_Name;
      begin
         R := new Rec'(Disc => 3, Comp => new Arr'(1 .. 3 => 5));
         R := F (R);
         R := new Rec'(Disc => 4, Comp => new Arr'(1 .. 4 => 5));
         R := F (R);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY FUNCTION FOR RECORD");
      exception
         when Constraint_Error =>
            if R = null or else R.Disc /= 4 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                  "ACCESS VALUE - RECORD,FUNCTION");
            end if;
      end;

      declare
         R : Rec_Name;
      begin
         R := new Rec'(Disc => 3, Comp => new Arr'(1 .. 3 => 5));
         Fproc (R);
         R := new Rec'(Disc => 4, Comp => new Arr'(1 .. 4 => 5));
         Fproc (R);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY PROCEDURE FOR RECORD");
      exception
         when Constraint_Error =>
            if R = null or else R.Disc /= 4 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                  "ACCESS VALUE - RECORD,PROCEDURE");
            end if;
      end;

      declare
         A : Arr_Name;
      begin
         A := new Arr'(1 .. 3 => 5);
         A := G (A);
         A := new Arr'(1 .. 4 => 6);
         A := G (A);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY FUNCTION FOR ARRAY");
      exception
         when Constraint_Error =>
            if A = null or else A (4) /= 6 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                  "ACCESS VALUE - ARRAY,FUNCTION");
            end if;
      end;

      declare
         A : Arr_Name;
      begin
         A := new Arr'(1 .. 3 => 5);
         Gproc (A);
         A := new Arr'(1 .. 4 => 6);
         Gproc (A);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY PROCEDURE FOR ARRAY");
      exception
         when Constraint_Error =>
            if A = null or else A (4) /= 6 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                  "ACCESS VALUE - ARRAY,PROCEDURE");
            end if;
      end;
   end;

   declare
      C3 : constant Integer := Ident_Int (3);

      type Rec (Disc : Integer) is record
         null;
      end record;

      type P_Arr is array (Integer range <>) of Integer;
      type P_Arr_Name is access P_Arr;

      type P_Rec_Name is access Rec;

      generic
         type Uncon_Arr is array (Integer range <>) of Integer;
      package P is
         type Acc_Rec is access Rec;
         type Acc_Arr is access Uncon_Arr;
         type Acc_P_Arr is access P_Arr;
         subtype Acc_P_Arr_3 is Acc_P_Arr (1 .. 3);
         Obj : Acc_Rec (C3);

         type Arr2 is array (1 .. 10) of Acc_Rec (C3);

         type Rec1 is record
            Comp1 : Acc_Rec (C3);
         end record;

         type Rec2 is record
            Comp2 : Acc_Arr (1 .. C3);
         end record;

         subtype Acc_Rec_3 is Acc_Rec (C3);

         function F (Parm : Acc_Rec_3) return Acc_Rec_3;

         procedure Fproc (Parm : Acc_Rec_3);

         function G (Pa : Acc_P_Arr_3) return Acc_P_Arr_3;

         procedure Gproc (Pa : Acc_P_Arr_3);

         type Acc1 is private;
         type Acc2 is private;
         type Der1 is private;
         type Der2 is private;

      private

         type Acc1 is access Acc_Rec (C3);
         type Acc2 is access Acc_Arr (1 .. C3);
         type Der1 is new Acc_Rec (C3);
         type Der2 is new Acc_Arr (1 .. C3);
      end P;

      package body P is
         function F (Parm : Acc_Rec_3) return Acc_Rec_3 is
         begin
            if not Equal (Ident_Int (3), 1 + Ident_Int (2)) then
               Comment ("DON'T OPTIMIZE F AWAY");
            end if;
            return Parm;
         end F;

         procedure Fproc (Parm : Acc_Rec_3) is
         begin
            if not Equal (Ident_Int (4), 2 + Ident_Int (2)) then
               Comment ("DON'T OPTIMIZE FPROC AWAY");
            end if;
         end Fproc;

         function G (Pa : Acc_P_Arr_3) return Acc_P_Arr_3 is
         begin
            if not Equal (Ident_Int (5), 3 + Ident_Int (2)) then
               Comment ("DON'T OPTIMIZE G AWAY");
            end if;
            return Pa;
         end G;

         procedure Gproc (Pa : Acc_P_Arr_3) is
         begin
            if not Equal (Ident_Int (6), 4 + Ident_Int (2)) then
               Comment ("DON'T OPTIMIZE GPROC AWAY");
            end if;
         end Gproc;
      end P;

      package Np is new P (Uncon_Arr => P_Arr);

      use Np;

   begin
      declare
         R : Acc_Rec;
      begin
         R := new Rec (Disc => 3);
         R := F (R);
         R := new Rec (Disc => 4);
         R := F (R);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY FUNCTION FOR A RECORD -GENERIC");
      exception
         when Constraint_Error =>
            if R = null or else R.Disc /= 4 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT " &
                  "OF ACCESS VALUE - RECORD," & "FUNCTION -GENERIC");
            end if;
      end;

      declare
         R : Acc_Rec;
      begin
         R := new Rec (Disc => 3);
         Fproc (R);
         R := new Rec (Disc => 4);
         Fproc (R);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY PROCEDURE FOR A RECORD -GENERIC");
      exception
         when Constraint_Error =>
            if R = null or else R.Disc /= 4 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT " &
                  "OF ACCESS VALUE - RECORD," & "PROCEDURE -GENERIC");
            end if;
      end;

      declare
         A : Acc_P_Arr;
      begin
         A := new P_Arr'(1 .. 3 => 5);
         A := G (A);
         A := new P_Arr'(1 .. 4 => 6);
         A := G (A);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY FUNCTION FOR AN ARRAY -GENERIC");
      exception
         when Constraint_Error =>
            if A = null or else A (4) /= 6 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT " &
                  "OF ACCESS VALUE - ARRAY," & "FUNCTION -GENERIC");
            end if;
      end;

      declare
         A : Acc_P_Arr;
      begin
         A := new P_Arr'(1 .. 3 => 5);
         Gproc (A);
         A := new P_Arr'(1 .. 4 => 6);
         Gproc (A);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY PROCEDURE FOR AN ARRAY -GENERIC");
      exception
         when Constraint_Error =>
            if A = null or else A (4) /= 6 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT " &
                  "OF ACCESS VALUE - ARRAY," & "PROCEDURE -GENERIC");
            end if;
      end;
   end;

   declare
      type Con_Int is range 1 .. 10;

      generic
         type Uncon_Int is range <>;
      package P2 is
         subtype New_Int is Uncon_Int range 1 .. 5;
         function Func_Int (Parm : New_Int) return New_Int;

         procedure Proc_Int (Parm : New_Int);
      end P2;

      package body P2 is
         function Func_Int (Parm : New_Int) return New_Int is
         begin
            if not Equal (Ident_Int (3), 1 + Ident_Int (2)) then
               Comment ("DON'T OPTIMIZE F AWAY");
            end if;
            return Parm;
         end Func_Int;

         procedure Proc_Int (Parm : New_Int) is
         begin
            if not Equal (Ident_Int (4), 2 + Ident_Int (2)) then
               Comment ("DON'T OPTIMIZE FPROC AWAY");
            end if;
         end Proc_Int;
      end P2;

      package Np2 is new P2 (Uncon_Int => Con_Int);

      use Np2;

   begin
      declare
         R : Con_Int;
      begin
         R := 2;
         R := Func_Int (R);
         R := 8;
         R := Func_Int (R);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON VALUE " &
            "ACCEPTED BY FUNCTION -GENERIC");
      exception
         when Constraint_Error =>
            if R /= 8 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT " &
                  "OF VALUE -FUNCTION, GENERIC");
            end if;
      end;

      declare
         R : Con_Int;
      begin
         R := 2;
         Proc_Int (R);
         R := 9;
         Proc_Int (R);
         Failed
           ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
            "ACCEPTED BY PROCEDURE -GENERIC");
      exception
         when Constraint_Error =>
            if R /= 9 then
               Failed
                 ("ERROR IN EVALUATION/ASSIGNMENT " &
                  "OF ACCESS VALUE - PROCEDURE, " & "GENERIC");
            end if;
      end;
   end;

   Result;
end C38002a;

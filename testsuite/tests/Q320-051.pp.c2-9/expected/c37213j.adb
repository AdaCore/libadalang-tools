-- C37213J.ADA

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
--     CHECK, FOR A GENERIC FORMAL TYPE - WHERE A DISCRIMINANT OR AN
--     INDEX CONSTRAINT DEPENDS ON A RECORD DISCRIMINANT AND THE
--     RECORD TYPE IS CONSTRAINED BY DEFAULT - USED TO DECLARE AN
--     OBJECT OR A SUBTYPE, THAT THE NON-DISCRIMINANT EXPRESSIONS
--     OF THE CONSTRAINT ARE CHECKED FOR COMPATIBILITY:
--          1) ONLY IN AN OBJECT DECLARATION, AND
--          2) ONLY IF THE DISCRIMINANT-DEPENDENT COMPONENT IS PRESENT
--             IN THE SUBTYPE.

-- HISTORY:
--     JBG  10/17/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER; SEPARATED THIS TEST INTO
--                    3 NEW TESTS (J,K,L); CHANGED THE AGGREGATE FOR
--                    THE PARAMETER 'VALUE' IN THE CALL OF PROCEDURE
--                    'SUBTYPE_CHK1';  MOVED THE CALL TO REPORT.TEST
--                    SO THAT IT COMES BEFORE ANY DECLARATIONS; ADDED
--                    A SEQUENCE COUNTER TO IDENTIFY WHICH SUBTEST
--                    DECLARATION PART RAISES CONSTRAINT_ERROR.
--     VCL  03/28/88  MODIFIED THE TEST DISCRIPTION TO MORE ACCURATELY
--                    DESCRIBE THE OBJECTIVE; CHANGED THE FORMAL
--                    PARAMETERS TO THE GENERIC UNITS AND THE
--                    CORRESPONDING ACTUAL PARAMETERS; REORGANIZED THE
--                    TEST SO THAT ALL OPERATIONS ON A SPECIFIC TYPE
--                    ARE TOGETHER.

with Report; use Report;
procedure C37213j is
begin
   Test
     ("C37213J",
      "THE NON-DISCRIMINANT VALUES OF A DISCRIMINANT " &
      "OR AN INDEX CONSTRAINT THAT DEPEND ON A " &
      "DISCRIMINANT ARE PROPERLY CHECKED WHEN THE " &
      "RECORD TYPE IS CONSTRAINED BY DEFAULT AND " &
      "USED AS THE ACTUAL PARAMETER TO A GENERIC " &
      "FORMAL TYPE USED TO DECLARE AN OBJECT OR A " &
      "SUBTYPE");

   declare
      subtype Sm is Integer range 1 .. 10;
      type Rec (D1, D2 : Sm) is record
         null;
      end record;
      type My_Arr is array (Sm range <>) of Integer;

      Sequence_Number : Integer;

      generic
         type Cons is private;
         Obj_Xcp : Boolean;
         Tag : String;
      package Obj_Chk is
      end Obj_Chk;

      generic
         type Cons is private;
      procedure Subtyp_Chk (Obj_Xcp : Boolean; Tag : String);

      package body Obj_Chk is
      begin          -- DECLARE AN OBJECT OF THE FORMAL TYPE.
         declare
            X : Cons;

            function Value return Cons is
            begin
               if Equal (3, 3) then
                  return X;
               else
                  return X;
               end if;
            end Value;
         begin
            if Obj_Xcp then
               Failed
                 ("NO CHECK DURING DECLARATION " &
                  "OF OBJECT OF TYPE CONS - " &
                  Tag);
            elsif X /= Value then
               Failed
                 ("INCORRECT VALUE FOR OBJECT OF " & "TYPE CONS - " & Tag);
            end if;
         end;
      exception
         when Constraint_Error =>
            if not Obj_Xcp then
               Failed
                 ("IMPROPER CONSTRAINT CHECKED " &
                  "DURING DECLARATION OF OBJECT " &
                  "OF TYPE CONS - " &
                  Tag);
            end if;
      end Obj_Chk;

      procedure Subtyp_Chk (Obj_Xcp : Boolean; Tag : String) is
      begin          -- DECLARE A SUBTYPE OF THE FORMAL TYPE.
         declare
            subtype Scons is Cons;
         begin
            declare
               X : Scons;

               function Value return Scons is
               begin
                  if Equal (5, 5) then
                     return X;
                  else
                     return X;
                  end if;
               end Value;
            begin
               if Obj_Xcp then
                  Failed
                    ("NO CHECK DURING DECLARATION " &
                     "OF OBJECT OF SUBTYPE SCONS - " &
                     Tag);
               elsif X /= Value then
                  Failed
                    ("INCORRECT VALUE FOR OBJECT " &
                     "OF SUBTYPE SCONS - " &
                     Tag);
               end if;
            end;
         exception
            when Constraint_Error =>
               if not Obj_Xcp then
                  Failed
                    ("IMPROPER CONSTRAINT CHECKED " &
                     "DURING DECLARATION OF OBJECT " &
                     "OF SUBTYPE SCONS - " &
                     Tag);
               end if;
         end;
      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT IMPROPERLY CHECKED " &
               "DURING SUBTYPE DECLARATION - " &
               Tag);
      end Subtyp_Chk;
   begin
      Sequence_Number := 1;
      declare
         type Rec_Def (D3 : Integer := 1) is record
            C1 : Rec (D3, 0);
         end record;

         package Pack1 is new Obj_Chk
           (Rec_Def,
            Obj_Xcp => True,
            Tag     => "PACK1");

         procedure Proc1 is new Subtyp_Chk (Rec_Def);
      begin
         Proc1 (Obj_Xcp => True, Tag => "PROC1");
      end;

      Sequence_Number := 2;
      declare
         type Arr_Def (D3 : Integer := Ident_Int (1)) is record
            C1 : My_Arr (0 .. D3);
         end record;

         package Pack2 is new Obj_Chk
           (Arr_Def,
            Obj_Xcp => True,
            Tag     => "PACK2");

         procedure Proc2 is new Subtyp_Chk (Arr_Def);
      begin
         Proc2 (Obj_Xcp => True, Tag => "PROC2");
      end;

      Sequence_Number := 3;
      declare
         type Var_Rec_Def1 (D3 : Integer := 1) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : Rec (D3, Ident_Int (11));
               when others =>
                  C2 : Integer := Ident_Int (5);
            end case;
         end record;

         package Pack3 is new Obj_Chk
           (Var_Rec_Def1,
            Obj_Xcp => True,
            Tag     => "PACK3");

         procedure Proc3 is new Subtyp_Chk (Var_Rec_Def1);
      begin
         Proc3 (Obj_Xcp => True, Tag => "PROC3");
      end;

      Sequence_Number := 4;
      declare
         type Var_Rec_Def6 (D3 : Integer := Ident_Int (-6)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : Rec (D3, Ident_Int (11));
               when others =>
                  C2 : Integer := Ident_Int (5);
            end case;
         end record;

         package Pack4 is new Obj_Chk
           (Var_Rec_Def6,
            Obj_Xcp => False,
            Tag     => "PACK4");

         procedure Proc4 is new Subtyp_Chk (Var_Rec_Def6);
      begin
         Proc4 (Obj_Xcp => False, Tag => "PROC4");
      end;

      Sequence_Number := 5;
      declare
         type Var_Rec_Def11 (D3 : Integer := 11) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : Rec (D3, Ident_Int (11));
               when others =>
                  C2 : Integer := Ident_Int (5);
            end case;
         end record;

         package Pack5 is new Obj_Chk
           (Var_Rec_Def11,
            Obj_Xcp => False,
            Tag     => "PACK5");

         procedure Proc5 is new Subtyp_Chk (Var_Rec_Def11);
      begin
         Proc5 (Obj_Xcp => False, Tag => "PROC5");
      end;

      Sequence_Number := 6;
      declare
         type Var_Arr_Def1 (D3 : Integer := Ident_Int (1)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (D3 .. Ident_Int (11));
               when others =>
                  C2 : Integer := Ident_Int (5);
            end case;
         end record;

         package Pack6 is new Obj_Chk
           (Var_Arr_Def1,
            Obj_Xcp => True,
            Tag     => "PACK6");

         procedure Proc6 is new Subtyp_Chk (Var_Arr_Def1);
      begin
         Proc6 (Obj_Xcp => True, Tag => "PROC6");
      end;

      Sequence_Number := 7;
      declare
         type Var_Arr_Def6 (D3 : Integer := -6) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (D3 .. Ident_Int (11));
               when others =>
                  C2 : Integer := Ident_Int (5);
            end case;
         end record;

         package Pack7 is new Obj_Chk
           (Var_Arr_Def6,
            Obj_Xcp => False,
            Tag     => "PACK7");

         procedure Proc7 is new Subtyp_Chk (Var_Arr_Def6);
      begin
         Proc7 (Obj_Xcp => False, Tag => "PROC7");
      end;

      Sequence_Number := 8;
      declare
         type Var_Arr_Def11 (D3 : Integer := Ident_Int (11)) is record
            case D3 is
               when -5 .. 10 =>
                  C1 : My_Arr (D3 .. Ident_Int (11));
               when others =>
                  C2 : Integer := Ident_Int (5);
            end case;
         end record;

         package Pack8 is new Obj_Chk
           (Var_Arr_Def11,
            Obj_Xcp => False,
            Tag     => "PACK8");

         procedure Proc8 is new Subtyp_Chk (Var_Arr_Def11);
      begin
         Proc8 (Obj_Xcp => False, Tag => "PROC8");
      end;

   exception
      when others =>
         Failed
           ("EXCEPTION RAISED DURING DECLARATION / " &
            "INSTANTIATION ELABORATION - " &
            Integer'Image (Sequence_Number));
   end;

   Result;
end C37213j;

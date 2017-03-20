-- C37405A.ADA

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
-- CHECK THAT WHEN ASSIGNING TO A CONSTRAINED OR UNCONSTRAINED
-- OBJECT OR FORMAL PARAMETER OF A TYPE DECLARED WITH DEFAULT
-- DISCRIMINANTS, THE ASSIGNMENT DOES NOT CHANGE THE 'CONSTRAINED
-- ATTRIBUTE VALUE ASSOCIATED WITH THE OBJECT OR PARAMETER.

-- ASL 7/21/81
-- TBN 1/20/86     RENAMED FROM C37209A.ADA AND REVISED THE ASSIGNMENTS
--                 OF CONSTRAINED AND UNCONSTRAINED OBJECTS TO ARRAY AND
--                 RECORD COMPONENTS.

with Report; use Report;
procedure C37405a is

   type Rec (Disc : Integer := 25) is record
      Comp : Integer;
   end record;

   subtype Constr is Rec (10);
   subtype Unconstr is Rec;

   type Rec_C is record
      Comp : Constr;
   end record;

   type Rec_U is record
      Comp : Unconstr;
   end record;

   C1, C2 : Constr;
   U1, U2 : Unconstr;
-- C2 AND U2 ARE NOT PASSED TO EITHER PROC1 OR PROC2.

   Arr_C : array (1 .. 5) of Constr;
   Arr_U : array (1 .. 5) of Unconstr;

   Rec_Comp_C : Rec_C;
   Rec_Comp_U : Rec_U;

   procedure Proc11 (Parm : in out Unconstr; B : in Boolean) is
   begin
      Parm := C2;
      if Ident_Bool (B) /= Parm'Constrained then
         Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY " & "ASSIGNMENT - 1");
      end if;
   end Proc11;

   procedure Proc12 (Parm : in out Unconstr; B : in Boolean) is
   begin
      Parm := U2;
      if B /= Parm'Constrained then
         Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY " & "ASSIGNMENT - 2");
      end if;
   end Proc12;

   procedure Proc1 (Parm : in out Unconstr; B : in Boolean) is
   begin
      if B /= Parm'Constrained then
         Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY " & "PASSING PARAMETER");
      end if;

      Proc11 (Parm, B);

      Proc12 (Parm, B);

   end Proc1;

   procedure Proc2 (Parm : in out Constr) is
   begin
      Comment ("CALLING PROC1 FROM PROC2");   -- IN CASE TEST FAILS.
      Proc1 (Parm, True);
      Parm := U2;
      if not Parm'Constrained then
         Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY " & "ASSIGNMENT - 3");
      end if;
   end Proc2;
begin
   Test
     ("C37405A",
      "'CONSTRAINED ATTRIBUTE OF OBJECTS, FORMAL " &
      "PARAMETERS CANNOT BE CHANGED BY ASSIGNMENT");

   C2 := (Disc => Ident_Int (10), Comp => 3);
   U2 := (Disc => Ident_Int (10), Comp => 4);

   Arr_C := (1 .. 5 => U2);
   Arr_U := (1 .. 5 => C2);

   Rec_Comp_C := (Comp => U2);
   Rec_Comp_U := (Comp => C2);

   C1 := U2;
   U1 := C2;

   if U1'Constrained or not C1'Constrained then
      Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY ASSIGNMENT - 4");
   end if;

   if Arr_U (3)'Constrained or not Arr_C (4)'Constrained then
      Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY ASSIGNMENT - 5");
   end if;

   if Rec_Comp_U.Comp'Constrained or not Rec_Comp_C.Comp'Constrained then
      Failed ("'CONSTRAINED ATTRIBUTE CHANGED BY ASSIGNMENT - 6");
   end if;

   Comment ("CALLING PROC1 DIRECTLY");
   Proc1 (C1, True);
   Proc2 (C1);

   Comment ("CALLING PROC1 DIRECTLY");
   Proc1 (U1, False);
   Proc2 (U1);

   Comment ("CALLING PROC1 DIRECTLY");
   Proc1 (Arr_C (4), True);
   Proc2 (Arr_C (5));

   Comment ("CALLING PROC1 DIRECTLY");
   Proc1 (Arr_U (2), False);
   Proc2 (Arr_U (3));

   Comment ("CALLING PROC1 DIRECTLY");
   Proc1 (Rec_Comp_C.Comp, True);
   Proc2 (Rec_Comp_C.Comp);

   Comment ("CALLING PROC1 DIRECTLY");
   Proc1 (Rec_Comp_U.Comp, False);
   Proc2 (Rec_Comp_U.Comp);

   Result;
end C37405a;

-- C37208A.ADA (RA #534/1)

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
-- FOR A TYPE WITH DEFAULT DISCRIMINANT VALUES, CHECK THAT A DISCRIMINANT
-- CONSTRAINT CAN BE OMITTED IN:

-- AN OBJECT DECLARATION, AND HENCE ASSIGNMENTS TO THE OBJECT CAN CHANGE ITS
-- DISCRIMINANTS;

-- A COMPONENT_DECLARATION IN A RECORD TYPE DEFINITION, AND HENCE ASSIGNMENTS
-- TO THE COMPONENT CAN CHANGE THE VALUE OF ITS DISCRIMINANTS;

-- A SUBTYPE INDICATION IN AN ARRAY TYPE DEFINITION, AND HENCE ASSIGNMENTS TO
-- ONE OF THE COMPONENTS CAN CHANGE ITS DISCRIMINANT VALUES;

-- A FORMAL PARAMETER OF A SUBPROGRAM; EXCEPT FOR PARAMETERS OF MODE IN, THE
-- 'CONSTRAINED ATTRIBUTE OF THE ACTUAL PARAMETER BECOMES THE 'CONSTRAINED
-- ATTRIBUTE OF THE FORMAL PARAMETER; FOR IN OUT AND OUT PARAMETERS, IF THE
-- 'CONSTRAINED ATTRIBUTE IS FALSE, ASSIGNMENTS TO THE FORMAL PARAMETER CAN
-- CHANGE THE DISCRIMINANTS OF THE ACTUAL PARAMETER; IF THE 'CONSTRAINED
-- ATTRIBUTE IS TRUE, ASSIGNNMENTS THAT ATTEMPT TO CHANGE THE DISCRIMINANTS
-- OF THE ACTUAL PARAMETER RAISE CONSTRAINT_ERROR.

-- ASL 7/23/81
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report;
procedure C37208a is

   use Report;

begin
   Test
     ("C37208A",
      "DISCRIMINANT CONSTRAINT CAN BE OMITTED " &
      "FROM OBJECT DECLARATION, COMPONENT DECLARATION, SUBTYPE " &
      "INDICATION OR FORMAL SUBPROGRAM PARAMETER, IF THE TYPE " &
      "HAS DEFAULT DISCRIMINANTS");

   declare
      type Rec1 (Disc : Integer := 7) is record
         null;
      end record;

      type Rec2 is record
         Comp : Rec1;
      end record;

      R          : Rec2;
      U1, U2, U3 : Rec1     := (Disc => 3);
      C1, C2, C3 : Rec1 (3) := (Disc => 3);
      Arr        : array (Integer range 1 .. 10) of Rec1;
      Arr2       : array (1 .. 10) of Rec1 (4);

      procedure Proc
        (P_In     : in     Rec1;
         P_Out    :    out Rec1;
         P_In_Out : in out Rec1;
         Constr   : in     Boolean)
      is
      begin
         if P_Out'Constrained /= Constr or P_In_Out'Constrained /= Constr then
            Failed
              ("CONSTRAINED ATTRIBUTES DO NOT MATCH " &
               "FOR ACTUAL AND FORMAL PARAMETERS");
         end if;

         if P_In'Constrained /= Ident_Bool (True) then
            Failed ("'CONSTRAINED IS FALSE FOR IN " & "PARAMETER");
         end if;

         if not Constr then     -- UNCONSTRAINED ACTUAL PARAM
            P_Out    := (Disc => Ident_Int (0));
            P_In_Out := (Disc => Ident_Int (0));
         else
            begin
               P_Out := (Disc => Ident_Int (0));
               Failed
                 ("DISCRIMINANT OF CONSTRAINED ACTUAL " &
                  "PARAMETER ILLEGALLY CHANGED - 1");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION - 1");
            end;

            begin
               P_In_Out := (Disc => Ident_Int (0));
               Failed
                 ("DISCRIMINANT OF CONSTRAINED ACTUAL " &
                  "PARAMETER ILLEGALLY CHANGED - 2");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION - 2");
            end;
         end if;
      end Proc;
   begin
      if U1.Disc /= Ident_Int (3) then
         Failed ("INITIAL DISCRIMINANT VALUE WRONG - U1");
      end if;

      U1 := (Disc => Ident_Int (5));
      if U1.Disc /= 5 then
         Failed ("ASSIGNMENT FAILED FOR OBJECT");
      end if;

      if R.Comp.Disc /= Ident_Int (7) then
         Failed ("DEFAULT DISCRIMINANT VALUE WRONG - R");
      end if;

      R.Comp := (Disc => Ident_Int (5));
      if R.Comp.Disc /= 5 then
         Failed ("ASSIGNMENT FAILED FOR RECORD COMPONENT");
      end if;

      for I in 1 .. 10 loop
         if Arr (I).Disc /= Ident_Int (7) then
            Failed ("DEFAULT DISCRIMINANT VALUE WRONG - ARR");
         end if;
      end loop;

      Arr (3) := (Disc => Ident_Int (5));
      if Arr (3).Disc /= 5 then
         Failed ("ASSIGNMENT FAILED FOR ARRAY COMPONENT");
      end if;

      if Arr /= (1 .. 2 | 4 .. 10 => (Disc => 7), 3 => (Disc => 5)) then
         Failed ("MODIFIED WRONG COMPONENTS");
      end if;

      Proc (C1, C2, C3, Ident_Bool (True));
      Proc (U1, U2, U3, Ident_Bool (False));
      if U2.Disc /= 0 or U3.Disc /= 0 then
         Failed
           ("ASSIGNMENT TO UNCONSTRAINED ACTUAL PARAMETER " &
            "FAILED TO CHANGE DISCRIMINANT");
      end if;

      Proc (Arr (1), Arr (3), Arr (4), False);
      if Arr (3).Disc /= 0 or Arr (4).Disc /= 0 then
         Failed
           ("ARRAY COMPONENT ASSIGNMENTS DIDN'T CHANGE " &
            "DISCRIMINANT OF COMPONENT");
      end if;

      Proc (Arr2 (2), Arr2 (5), Arr2 (10), True);
   end;

   Result;
end C37208a;

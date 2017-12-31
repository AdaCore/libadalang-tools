-- C37208B.ADA

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
-- CONSTRAINT CAN BE OMITTED IN A GENERIC FORMAL PARAMETER, AND HENCE, FOR BOTH
-- IN AND IN OUT PARAMETERS, THE 'CONSTRAINED ATTRIBUTE OF THE ACTUAL PARAMETER
-- BECOMES THE 'CONSTRAINED ATTRIBUTE OF THE FORMAL PARAMETER, AND, FOR IN
-- OUT PARAMETERS, IF THE 'CONSTRAINED ATTRIBUTE IS FALSE, ASSIGNMENTS TO THE
-- FORMAL PARAMETERS CAN CHANGE THE DISCRIMINANTS OF THE ACTUAL PARAMETER; IF
-- THE 'CONSTRAINED ATTRIBUTE IS TRUE, ASSIGNMENTS THAT ATTEMPT TO CHANGE THE
-- DISCRIMINANTS OF THE ACTUAL PARAMETER RAISE CONSTRAINT_ERROR.

-- ASL 7/29/81
-- VKG 1/20/83
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report;
procedure C37208b is

   use Report;

begin
   Test
     ("C37208B",
      "FOR TYPES WITH DEFAULT DISCRIMINANT " &
      "VALUES, DISCRIMINANT CONSTRAINTS CAN BE OMITTED " &
      "IN GENERIC FORMAL PARAMETERS, AND THE " &
      "'CONSTRAINED ATTRIBUTE HAS CORRECT VALUES " &
      "DEPENDING ON THE ACTUAL PARAMETERS");

   declare
      type Rec (Disc : Integer := 7) is record
         null;
      end record;

      Kc           : constant Rec (3) := (Disc => 3);
      Ku           : constant Rec     := (Disc => 3);
      Objc1, Objc2 : Rec (3)          := (Disc => 3);
      Obju1, Obju2 : Rec              := (Disc => 3);

      generic
         P_In1 : Rec;
         P_In2 : Rec;
         P_In_Out : in out Rec;
         Status : Boolean;
      procedure Proc;

      procedure Proc is
      begin

         if P_In1'Constrained /= True or P_In2'Constrained /= True or
           P_In_Out'Constrained /= Status then

            Failed
              ("'CONSTRAINED ATTRIBUTES DO NOT MATCH " &
               "FOR ACTUAL AND FORMAL PARAMETERS");
         end if;
         if not Status then
            begin
               P_In_Out := (Disc => Ident_Int (7));
            exception
               when others =>
                  Failed
                    ("EXCEPTION RAISED " & "WHEN TRYING TO " &
                     "CHANGE UNCONSTRAINED " & "DISCRIMINANT VALUE");
            end;
         else
            begin
               P_In_Out := (Disc => Ident_Int (7));
               Failed
                 ("DISCRIMINANT OF CONSTRAINED " &
                  "ACTUAL PARAMETER ILLEGALLY " & "CHANGED BY ASSIGNMENT");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION");
            end;
         end if;
      end Proc;

   begin

      declare
         procedure Proc_C is new Proc (Kc, Objc1, Objc2, Ident_Bool (True));
         procedure Proc_U is new Proc (Ku, Obju1, Obju2, Ident_Bool (False));
      begin
         Proc_C;
         Proc_U;
         if Obju2.Disc /= 7 then
            Failed
              ("ASSIGNMENT TO UNCONSTRAINED ACTUAL " &
               "PARAMETER FAILED TO CHANGE DISCRIMINANT ");
         end if;
      end;

   end;
   Result;
end C37208b;

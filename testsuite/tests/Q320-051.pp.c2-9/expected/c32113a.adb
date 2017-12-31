-- C32113A.ADA

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
--     CHECK THAT WHEN A VARIABLE OR CONSTANT HAVING A CONSTRAINED TYPE
--     WITH DISCRIMINANTS IS DECLARED WITH AN INITIAL VALUE,
--     CONSTRAINT_ERROR IS RAISED IF THE CORRESPONDING DISCRIMINANTS OF
--     THE INITIAL VALUE AND THE SUBTYPE DO NOT HAVE THE SAME VALUE.

-- HISTORY:
--     RJW 07/20/86
--     DWC 06/22/87  ADDED SUBTYPE PRIVAS.  ADDED CODE TO PREVENT DEAD
--                   VARIABLE OPTIMIZATION.

with Report; use Report;

procedure C32113a is

   package Pkg is
      type Priva (D : Integer := 0) is private;
      subtype Privas is Priva (Ident_Int (1));
      Pra1 : constant Privas;

      type Privb (D1, D2 : Integer) is private;
      Prb12 : constant Privb;

   private
      type Priva (D : Integer := 0) is record
         null;
      end record;

      type Privb (D1, D2 : Integer) is record
         null;
      end record;

      Pra1  : constant Privas := (D => (Ident_Int (1)));
      Prb12 : constant Privb  := (Ident_Int (1), Ident_Int (2));
   end Pkg;

   use Pkg;

   type Reca (D : Integer := 0) is record
      null;
   end record;

   type Recb (D1, D2 : Integer) is record
      null;
   end record;

   Ra1 : constant Reca (Ident_Int (1)) := (D => (Ident_Int (1)));

   Rb12 : constant Recb := (Ident_Int (1), Ident_Int (2));

begin
   Test
     ("C32113A",
      "CHECK THAT WHEN A VARIABLE OR CONSTANT " &
      "HAVING A CONSTRAINED TYPE IS DECLARED WITH " &
      "AN INITIAL VALUE, CONSTRAINT_ERROR IS " &
      "RAISED IF THE CORRESPONDING DISCRIMINANTS " &
      "OF THE INITIAL VALUE AND THE SUBTYPE DO " & "NOT HAVE THE SAME VALUE");

   begin
      declare
         Pr1 : constant Priva (Ident_Int (0)) := Pra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'PR1'");
         if Pr1 = Pra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'PR1'");
   end;

   begin
      declare
         Pr2 : constant Priva (Ident_Int (2)) := Pra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'PR2'");
         if Pr2 = Pra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'PR2'");
   end;

   begin
      declare
         Pr3 : Priva (Ident_Int (0)) := Pra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'PR3'");
         if Pr3 = Pra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'PR3'");
   end;

   begin
      declare
         Pr4 : Priva (Ident_Int (2)) := Pra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'PR4'");
         if Pr4 = Pra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'PR4'");
   end;

   begin
      declare
         subtype Spriva is Priva (Ident_Int (-1));
         Pr5 : constant Spriva := Pra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'PR5'");
         if Pr5 = Pra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'PR5'");
   end;

   begin
      declare
         subtype Spriva is Priva (Ident_Int (3));
         Pr6 : Spriva := Pra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'PR6'");
         if Pr6 = Pra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'PR6'");
   end;

   begin
      declare
         Pr7 : constant Privb (Ident_Int (1), Ident_Int (1)) := Prb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'PR7'");
         if Pr7 = Prb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'PR7'");
   end;

   begin
      declare
         Pr8 : constant Privb (Ident_Int (2), Ident_Int (2)) := Prb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'PR8'");
         if Pr8 = Prb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'PR8'");
   end;

   begin
      declare
         Pr9 : Privb (Ident_Int (1), Ident_Int (1)) := Prb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'PR9'");
         if Pr9 = Prb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'PR9'");
   end;

   begin
      declare
         Pr10 : Privb (Ident_Int (2), Ident_Int (2)) := Prb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'PR10'");
         if Pr10 = Prb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'PR10'");
   end;

   begin
      declare
         subtype Sprivb is Privb (Ident_Int (-1), Ident_Int (-2));
         Pr11 : constant Sprivb := Prb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'PR11'");
         if Pr11 = Prb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'PR11'");
   end;

   begin
      declare
         subtype Sprivb is Privb (Ident_Int (2), Ident_Int (1));
         Pr12 : Sprivb := Prb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'PR12'");
         if Pr12 = Prb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'PR12'");
   end;

   begin
      declare
         R1 : constant Reca (Ident_Int (0)) := Ra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R1'");
         if R1 = Ra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R1'");
   end;

   begin
      declare
         R2 : constant Reca (Ident_Int (2)) := Ra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R2'");
         if R2 = Ra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R2'");
   end;

   begin
      declare
         R3 : Reca (Ident_Int (0)) := Ra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R3'");
         if R3 = Ra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R3'");
   end;

   begin
      declare
         R4 : Reca (Ident_Int (2)) := Ra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R4'");
         if R4 = Ra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R4'");
   end;

   begin
      declare
         subtype Sreca is Reca (Ident_Int (-1));
         R5 : constant Sreca := Ra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R5'");
         if R5 = Ra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R5'");
   end;

   begin
      declare
         subtype Sreca is Reca (Ident_Int (3));
         R6 : Sreca := Ra1;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R6'");
         if R6 = Ra1 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R6'");
   end;

   begin
      declare
         R7 : constant Recb (Ident_Int (1), Ident_Int (1)) := Rb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R7'");
         if R7 = Rb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R7'");
   end;

   begin
      declare
         R8 : constant Recb (Ident_Int (2), Ident_Int (2)) := Rb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R8'");
         if R8 = Rb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R8'");
   end;

   begin
      declare
         R9 : Recb (Ident_Int (1), Ident_Int (1)) := Rb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R9'");
         if R9 = Rb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R9'");
   end;

   begin
      declare
         R10 : Recb (Ident_Int (2), Ident_Int (2)) := Rb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R10'");
         if R10 = Rb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'R10'");
   end;

   begin
      declare
         subtype Srecb is Recb (Ident_Int (-1), Ident_Int (-2));
         R11 : constant Srecb := Rb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'R11'");
         if R11 = Rb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'R11'");
   end;

   begin
      declare
         subtype Srecb is Recb (Ident_Int (2), Ident_Int (1));
         R12 : Srecb := Rb12;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'R12'");
         if R12 = Rb12 then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'R12'");
   end;

   Result;
end C32113a;

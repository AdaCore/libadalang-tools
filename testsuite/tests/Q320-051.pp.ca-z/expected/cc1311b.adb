-- CC1311B.ADA

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
--     CHECK THAT IF PARAMETERS OF DEFAULT AND FORMAL SUBPROGRAMS HAVE
--     THE SAME TYPE BUT NOT THE SAME SUBTYPE, THE PARAMETER SUBTYPES OF
--     THE SUBPROGRAM DENOTED BY THE DEFAULT ARE USED INSTEAD OF
--     SUBTYPES SPECIFIED IN THE FORMAL SUBPROGRAM DECLARATION.

-- HISTORY:
--     RJW 06/11/86 CREATED ORIGINAL TEST.
--     DHH 10/20/86 CORRECTED RANGE ERRORS.
--     PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.
--     PWN 10/27/95 REMOVED CHECKS AGAINST ARRAY SLIDING RULES THAT
--                  HAVE BEEN RELAXED.
--     PWN 10/25/96 RESTORED CHECKS WITH NEW ADA 95 EXPECTED RESULTS.

with Report; use Report;

procedure Cc1311b is

begin
   Test
     ("CC1311B",
      "CHECK THAT IF PARAMETERS OF DEFAULT AND " &
      "FORMAL SUBPROGRAMS HAVE THE SAME TYPE BUT " &
      "NOT THE SAME SUBTYPE, THE PARAMETER SUBTYPES " &
      "OF THE SUBPROGRAM DENOTED BY THE DEFAULT ARE " &
      "USED INSTEAD OF SUBTYPES SPECIFIED IN THE " &
      "FORMAL SUBPROGRAM DECLARATION");

   declare
      type Numbers is (Zero, One, Two);
      subtype Zero_Two is Numbers;
      subtype Zero_One is Numbers range Zero .. One;

      function Fsub (X : Zero_One) return Zero_One is
      begin
         return Numbers'Val (Ident_Int (Numbers'Pos (One)));
      end Fsub;

      generic
         with function F (X : Zero_Two := Two) return Zero_Two is Fsub;
      function Func return Zero_Two;

      function Func return Zero_Two is
      begin
         return F;
      exception
         when Constraint_Error =>
            return Zero;
         when others =>
            Failed ("WRONG EXCEPTION RAISED WITH " & "NFUNC1");
            return Zero;
      end Func;

      function Nfunc1 is new Func;

   begin
      if Nfunc1 = One then
         Failed ("NO EXCEPTION RAISED WITH NFUNC1");
      end if;
   end;

   declare
      type Gender is (Male, Female);

      type Person (Sex : Gender) is record
         case Sex is
            when Male =>
               Bearded : Boolean;
            when Female =>
               Children : Integer;
         end case;
      end record;

      subtype Man is Person (Sex => Male);
      subtype Testwriter is Person (Female);

      Rosa : Testwriter := (Female, 4);

      function F (X : Man) return Person is
         Tom : Person (Male) := (Male, False);
      begin
         if Equal (3, 3) then
            return X;
         else
            return Tom;
         end if;
      end F;

      generic
         type T is private;
         X1 : T;
         with function F (X : T) return T is <>;
      package Pkg is
      end Pkg;

      package body Pkg is
      begin
         if F (X1) = X1 then
            Failed
              ("NO EXCEPTION RAISED WITH " &
               "FUNCTION 'F' AND PACKAGE " &
               "'PKG' - 1");
         else
            Failed
              ("NO EXCEPTION RAISED WITH " &
               "FUNCTION 'F' AND PACKAGE " &
               "'PKG' - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WITH " &
               "FUNCTION 'F' AND PACKAGE 'PKG'");
      end Pkg;

      package Npkg is new Pkg (Testwriter, Rosa);

   begin
      Comment ("PACKAGE BODY ELABORATED - 1");
   end;

   declare
      type Vector is array (Positive range <>) of Integer;
      subtype Subv1 is Vector (1 .. 5);
      subtype Subv2 is Vector (2 .. 6);

      V1 : Subv1 := (1, 2, 3, 4, 5);

      function Fsub (Y : Subv2) return Vector is
         Z : Subv2;
      begin
         for I in Y'Range loop
            Z (I) := Ident_Int (Y (I));
         end loop;
         return Z;
      end Fsub;

      generic
         with function F (X : Subv1 := V1) return Subv1 is Fsub;
      procedure Proc;

      procedure Proc is
      begin
         if F = V1 then
            Comment
              ("NO EXCEPTION RAISED WITH " &
               "FUNCTION 'F' AND PROCEDURE " &
               "'PROC' - 1");
         else
            Comment
              ("NO EXCEPTION RAISED WITH " &
               "FUNCTION 'F' AND PROCEDURE " &
               "'PROC' - 2");
         end if;
      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED WITH " &
               "FUNCTION 'F' AND PROCEDURE " &
               "'PROC'");
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WITH " &
               "FUNCTION 'F' AND PROCEDURE " &
               "'PROC'");
      end Proc;

      procedure Nproc is new Proc;
   begin
      Nproc;
   end;

   declare

      type Acc is access String;

      subtype Index1 is Integer range 1 .. 5;
      subtype Index2 is Integer range 2 .. 6;

      subtype Acc1 is Acc (Index1);
      subtype Acc2 is Acc (Index2);

      Ac2 : Acc2 := new String'(2 .. 6 => 'A');
      Ac  : Acc;

      procedure P (Results : out Acc1; X : Acc1) is
      begin
         Results := null;
      end P;

      generic
         with procedure P1 (Results : out Acc2; X : Acc2 := Ac2) is P;
      function Func return Acc;

      function Func return Acc is
         Results : Acc;
      begin
         P1 (Results);
         return Results;
      exception
         when Constraint_Error =>
            return new String'("ABCDE");
         when others =>
            Failed ("WRONG EXCEPTION RAISED WITH " & "NFUNC2");
            return null;
      end Func;

      function Nfunc2 is new Func;

   begin
      Ac := Nfunc2;
      if Ac = null or else Ac.all /= "ABCDE" then
         Failed ("NO OR WRONG EXCEPTION RAISED WITH NFUNC2");
      end if;
   end;

   declare
      subtype Float1 is Float range -1.0 .. 0.0;
      subtype Float2 is Float range 0.0 .. 1.0;

      procedure Psub (Results : out Float2; X : Float2) is
      begin
         if Equal (3, 3) then
            Results := X;
         else
            Results := 0.0;
         end if;
      end Psub;

      generic
         with procedure P
           (Results : out Float1;
            X       :     Float1 := -0.062_5) is Psub;
      package Pkg is
      end Pkg;

      package body Pkg is
         Results : Float1;
      begin
         P (Results);
         if Results = 1.0 then
            Failed
              ("NO EXCEPTION RAISED WITH " &
               "PROCEDURE 'P' AND PACKAGE " &
               "'PKG' - 1");
         else
            Failed
              ("NO EXCEPTION RAISED WITH " &
               "PROCEDURE 'P' AND PACKAGE " &
               "'PKG' - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WITH " &
               "PROCEDURE 'P' AND PACKAGE 'PKG'");
      end Pkg;

      package Npkg is new Pkg;
   begin
      Comment ("PACKAGE BODY ELABORATED - 2");
   end;

   declare
      type Fixed is delta 0.125 range -1.0 .. 1.0;
      subtype Fixed1 is Fixed range -0.5 .. 0.0;
      subtype Fixed2 is Fixed range 0.0 .. 0.5;

      procedure P (Results : out Fixed1; X : Fixed1) is
      begin
         if Equal (3, 3) then
            Results := X;
         else
            Results := X;
         end if;
      end P;

      generic
         type F is delta <>;
         F1 : F;
         with procedure P (Results : out F; X : F) is <>;
      procedure Proc;

      procedure Proc is
         Results : F;
      begin
         P (Results, F1);
         if Results = 0.0 then
            Failed
              ("NO EXCEPTION RAISED WITH " &
               "PROCEDURE 'P' AND PROCEDURE " &
               "'PROC' - 1");
         else
            Failed
              ("NO EXCEPTION RAISED WITH " &
               "PROCEDURE 'P' AND PROCEDURE " &
               "'PROC' - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED WITH " &
               "PROCEDURE 'P' AND PROCEDURE " &
               "'PROC'");
      end Proc;

      procedure Nproc is new Proc (Fixed2, 0.125);

   begin
      Nproc;
   end;

   Result;

end Cc1311b;

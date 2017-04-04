-- CC3121A.ADA

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
-- CHECK THAT AN UNCONSTRAINED FORMAL GENERIC PARAMETER OF MODE "IN" HAVING AN
-- ARRAY TYPE OR A TYPE WITH DISCRIMINANTS HAS THE CONSTRAINTS OF THE ACTUAL
-- PARAMETER.

-- TBN  9/29/86

with Report; use Report;
procedure Cc3121a is

   subtype Int is Integer range 1 .. 10;

   type Array1 is array (Int range <>) of Integer;

   type Rec1 (D : Int) is record
      Var1 : Integer := 1;
   end record;

   type Rec2 (D : Int := 2) is record
      A : Array1 (D .. Ident_Int (4));
      B : Rec1 (D);
      C : Integer := 1;
   end record;

   type Array2 is array (Int range <>) of Rec2;

begin
   Test
     ("CC3121A",
      "CHECK THAT AN UNCONSTRAINED FORMAL GENERIC " &
      "PARAMETER OF MODE 'IN' HAVING AN ARRAY TYPE " &
      "OR A TYPE WITH DISCRIMINANTS HAS THE " &
      "CONSTRAINTS OF THE ACTUAL PARAMETER");

   declare
      Obj_Ara1 : Array1 (Ident_Int (2) .. 5);

      generic
         Var : Array1;
      procedure Proc;

      procedure Proc is
      begin
         if Var'First /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FOR VAR'FIRST");
         end if;
         if Var'Last /= Ident_Int (5) then
            Failed ("INCORRECT RESULTS FOR VAR'LAST");
         end if;
      end Proc;

      procedure Proc1 is new Proc (Obj_Ara1);
   begin
      Proc1;
   end;

   -------------------------------------------------------------------
   declare
      Obj_Rec2 : Rec2;

      generic
         Var : Rec2;
      function Func return Integer;

      function Func return Integer is
      begin
         if Var.D /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FROM VAR.D");
         end if;
         if Var.A'First /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FROM VAR.A'FIRST");
         end if;
         if Var.A'Last /= Ident_Int (4) then
            Failed ("INCORRECT RESULTS FROM VAR.A'LAST");
         end if;
         if Var.B.D /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FROM VAR.B.D");
         end if;
         return Ident_Int (1);
      end Func;

      function Func1 is new Func (Obj_Rec2);

   begin
      if Func1 /= Ident_Int (1) then
         Failed ("INCORRECT RESULTS FROM FUNC1 CALL");
      end if;
   end;

   -------------------------------------------------------------------
   declare
      Obj_Ara2 : Array2 (Ident_Int (6) .. 8);

      generic
         Var : Array2;
      procedure Proc;

      procedure Proc is
      begin
         if Var'First /= Ident_Int (6) then
            Failed ("INCORRECT RESULTS FOR VAR'FIRST");
         end if;
         if Var'Last /= Ident_Int (8) then
            Failed ("INCORRECT RESULTS FOR VAR'LAST");
         end if;
         if Var (6).D /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FROM VAR(6).D");
         end if;
         if Var (6).A'First /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FROM VAR(6).A'FIRST");
         end if;
         if Var (6).A'Last /= Ident_Int (4) then
            Failed ("INCORRECT RESULTS FROM VAR(6).A'LAST");
         end if;
         if Var (6).B.D /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FROM VAR(6).B.D");
         end if;
      end Proc;

      procedure Proc2 is new Proc (Obj_Ara2);
   begin
      Proc2;
   end;

   -------------------------------------------------------------------
   declare
      Obj_Rec3 : Rec2 (3);

      generic
         Var : Rec2;
      package Pac is
         Pac_Var : Integer := 1;
      end Pac;

      package body Pac is
      begin
         if Var.D /= Ident_Int (3) then
            Failed ("INCORRECT RESULTS FROM VAR.D");
         end if;
         if Var.A'First /= Ident_Int (3) then
            Failed ("INCORRECT RESULTS FROM VAR.A'FIRST");
         end if;
         if Var.A'Last /= Ident_Int (4) then
            Failed ("INCORRECT RESULTS FROM VAR.A'LAST");
         end if;
         if Var.B.D /= Ident_Int (3) then
            Failed ("INCORRECT RESULTS FROM VAR.B.D");
         end if;
      end Pac;

      package Pac1 is new Pac (Obj_Rec3);

   begin
      null;
   end;

   -------------------------------------------------------------------

   Result;
end Cc3121a;

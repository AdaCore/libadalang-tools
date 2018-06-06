-- C45112A.ADA

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
-- CHECK THAT THE BOUNDS OF THE RESULT OF A LOGICAL ARRAY OPERATION ARE THE
-- BOUNDS OF THE LEFT OPERAND.

-- RJW 2/3/86

with Report; use Report;

procedure C45112a is

   type Arr is array (Integer range <>) of Boolean;
   A1 : Arr (Ident_Int (3) .. Ident_Int (4)) := (True, False);
   A2 : Arr (Ident_Int (1) .. Ident_Int (2)) := (True, False);
   subtype Carr is Arr (Ident_Int (A1'First) .. Ident_Int (A1'Last));

   procedure Check (X : Arr; N1, N2 : String) is
   begin
      if X'First /= A1'First or X'Last /= A1'Last then
         Failed ("WRONG BOUNDS FOR " & N1 & " FOR " & N2);
      end if;
   end Check;

begin

   Test
     ("C45112A",
      "CHECK THE BOUNDS OF THE RESULT OF LOGICAL " & "ARRAY OPERATIONS");

   begin
      declare
         Aand : constant Arr := A1 and A2;
         Aor  : constant Arr := A1 or A2;
         Axor : constant Arr := A1 xor A2;
      begin
         Check (Aand, "INITIALIZATION OF CONSTANT ARRAY ", "'AND'");

         Check (Aor, "INITIALIZATION OF CONSTANT ARRAY ", "'OR'");

         Check (Axor, "INITIALIZATION OF CONSTANT ARRAY ", "'XOR'");
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED DURING " & "INTIALIZATIONS");
      when others =>
         Failed ("OTHER EXCEPTION RAISED DURING " & "INITIALIZATIONS");
   end;

   declare
      procedure Proc (A : Arr; Str : String) is
      begin
         Check (A, "FORMAL PARAMETER FOR CONSTRAINED ARRAY", Str);
      end Proc;
   begin
      Proc ((A1 and A2), "'AND'");
      Proc ((A1 or A2), "'OR'");
      Proc ((A1 xor A2), "'XOR'");
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING TEST FOR FORMAL " & "PARAMETERS");
   end;

   declare
      function Funcand return Arr is
      begin
         return A1 and A2;
      end Funcand;

      function Funcor return Arr is
      begin
         return A1 or A2;
      end Funcor;

      function Funcxor return Arr is
      begin
         return A1 xor A2;
      end Funcxor;

   begin
      Check (Funcand, "RETURN STATEMENT", "'AND'");
      Check (Funcor, "RETURN STATEMENT", "'OR'");
      Check (Funcxor, "RETURN STATEMENT", "'XOR'");

   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING TEST FOR RETURN " & "FROM FUNCTION");
   end;

   begin
      declare
         generic
            X : in Arr;
         package Pkg is
            function G return Arr;
         end Pkg;

         package body Pkg is
            function G return Arr is
            begin
               return X;
            end G;
         end Pkg;

         package Pand is new Pkg (X => A1 and A2);
         package Por is new Pkg (X => A1 or A2);
         package Pxor is new Pkg (X => A1 xor A2);
      begin
         Check (Pand.G, "GENERIC FORMAL PARAMETER", "'AND'");
         Check (Por.G, "GENERIC FORMAL PARAMETER", "'OR'");
         Check (Pxor.G, "GENERIC FORMAL PARAMMETER", "'XOR'");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING GENERIC " & "INSTANTIATION");
   end;

   declare
      type Acc is access Arr;
      Ac : Acc;

   begin
      Ac := new Arr'(A1 and A2);
      Check (Ac.all, "ALLOCATION", "'AND'");
      Ac := new Arr'(A1 or A2);
      Check (Ac.all, "ALLOCATION", "'OR'");
      Ac := new Arr'(A1 xor A2);
      Check (Ac.all, "ALLOCATION", "'XOR'");
   exception
      when others =>
         Failed ("EXCEPTION RAISED ON ALLOCATION");
   end;

   begin
      Check (Carr'(A1 and A2), "QUALIFIED EXPRESSION", "'AND'");
      Check (Carr'(A1 or A2), "QUALIFIED EXPRESSION", "'OR'");
      Check (Carr'(A1 xor A2), "QUALIFIED EXPRESSION", "'XOR'");
   exception
      when others =>
         Failed ("EXCEPTION RAISED ON QUALIFIED EXPRESSION");
   end;

   declare
      type Rec is record
         Rca : Carr;
      end record;
      R1 : Rec;

   begin
      R1 := (Rca => (A1 and A2));
      Check (R1.Rca, "AGGREGATE", "'AND'");
      R1 := (Rca => (A1 or A2));
      Check (R1.Rca, "AGGREGATE", "'OR'");
      R1 := (Rca => (A1 xor A2));
      Check (R1.Rca, "AGGREGATE", "'XOR'");
   exception
      when others =>
         Failed ("EXCEPTION RAISED ON AGGREGATE");
   end;

   begin
      declare
         type Recdef is record
            Rcdf1 : Carr := A1 and A2;
            Rcdf2 : Carr := A1 or A2;
            Rcdf3 : Carr := A1 xor A2;
         end record;
         Rd : Recdef;
      begin
         Check (Rd.Rcdf1, "DEFAULT RECORD", "'AND'");
         Check (Rd.Rcdf2, "DEFAULT RECORD", "'OR'");
         Check (Rd.Rcdf3, "DEFAULT RECORD", "'XOR'");
      exception
         when others =>
            Failed ("EXCEPTION RAISED ON DEFAULT RECORD");
      end;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED DURING INITIALIZATION OF " & "DEFAULT RECORD");
   end;

   declare
      procedure Pdef
        (X : Carr := A1 and A2; Y : Carr := A1 or A2; Z : Carr := A1 xor A2)
      is
      begin
         Check (X, "DEFAULT PARAMETER", "'AND'");
         Check (Y, "DEFAULT PARAMETER", "'OR'");
         Check (Z, "DEFAULT PARAMETER", "'XOR'");
      end Pdef;

   begin
      Pdef;
   exception
      when others =>
         Failed ("EXCEPTION RAISED ON DEFAULT PARM");
   end;

   Result;

end C45112a;

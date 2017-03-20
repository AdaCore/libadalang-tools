-- CC3601C.ADA

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
-- CHECK THAT "/=" MAY BE PASSED AS A GENERIC ACTUAL FUNCTION
-- PARAMETER.

-- DAT 10/6/81
-- SPS 10/27/82
-- JRK 2/9/83

with Report; use Report;

procedure Cc3601c is
begin
   Test ("CC3601C", "/= AS GENERIC ACTUAL PARAMETER");

   declare
      package Pk is
         type Lp is limited private;
         function "=" (X, Y : Lp) return Boolean;-- RETURNS FALSE.
         type Int is new Integer;
      private
         task type Lp;
      end Pk;
      use Pk;

      V1, V2 : Lp;

      type Rec is record
         C : Lp;
      end record;

      R1, R2 : Rec;

      type Int is new Integer;

      B1        : Boolean := True;
      B2        : Boolean := True;
      Integer_3 : Integer := 3;
      Integer_4 : Integer := 4;
      Int_3     : Int     := 3;
      Int_4     : Int     := 4;
      Int_5     : Int     := 5;
      Pk_Int_M1 : Pk.Int  := -1;
      Pk_Int_M2 : Pk.Int  := -2;
      Pk_Int_1  : Pk.Int  := 1;
      Pk_Int_2  : Pk.Int  := 2;
      Pk_Int_3  : Pk.Int  := 3;

      function "=" (Q, R : Lp) return Boolean;-- RETURNS TRUE.

      generic
         type T is limited private;
         V1, V2 : in out T;
         with function Ne (Za : in T; Zb : T) return Boolean;
         Value : in Boolean; -- SHOULD BE VALUE OF NE(V1,V2).
         Str : String;
      package Gp is
      end Gp;

      function "=" (Q, R : in Rec) return Boolean;

      function Ne (Q : Int; R : in Int) return Boolean renames "/=";

      function Ne (Q : Pk.Int; R : in Pk.Int) return Boolean renames "/=";

      package body Gp is
      begin
         if Ident_Bool (Value) /= Ne (V1, V2) then
            Failed ("WRONG /= ACTUAL GENERIC PARAMETER " & Str);
         end if;
      end Gp;

      function "=" (Q, R : in Rec) return Boolean is
      begin
         return False;
      end "=";

      function "=" (Q, R : Lp) return Boolean is
      begin
         return True;
      end "=";

      package body Pk is
         function "=" (X, Y : Lp) return Boolean is
         begin
            return R1 = R1;     -- FALSE.
         end "=";
         task body Lp is
         begin
            null;
         end Lp;
      end Pk;

      package P1 is new Gp (Lp, V1, V2, "/=", False, "1");

      function "NOT" (X : Boolean) return Boolean is
      begin
         return X;
      end "NOT"; -- ENSURES USE OF PREDEFINED "NOT"

      package P2 is new Gp (Lp, V1, V2, "/=", False, "2");
      package P3 is new Gp (Lp, V1, V2, Pk."/=", True, "3");
      package P4 is new Gp (Pk.Lp, V1, V2, "/=", False, "4");
      package P5 is new Gp (Pk.Lp, V1, V2, Pk."/=", True, "5");
      package P6 is new Gp (Rec, R1, R2, "/=", True, "6");
      package P7 is new Gp (Integer, Integer_3, Integer_4, "/=", True, "7");
      package P8 is new Gp (Boolean, B1, B2, "/=", False, "8");
      package P9 is new Gp (Int, Int_3, Int_5, "/=", True, "9");
      package P10 is new Gp (Int, Int_3, Int_3, "/=", False, "10");
      package P11 is new Gp (Int, Int_3, Int_4, Ne, True, "11");
      package P12 is new Gp (Int, Int_3, Int_3, Ne, False, "12");
      package P13 is new Gp (Pk.Int, Pk_Int_3, Pk_Int_3, Ne, False, "13");
      package P14 is new Gp (Pk.Int, Pk_Int_M1, Pk_Int_M2, Ne, True, "14");
      package P15 is new Gp (Pk.Int, Pk_Int_1, Pk_Int_1, "/=", False, "15");
      package P16 is new Gp (Pk.Int, Pk_Int_1, Pk_Int_2, "/=", True, "16");
      package P17 is new Gp (Pk.Int, Pk_Int_1, Pk_Int_1, Pk."/=", False, "17");
      package P18 is new Gp (Pk.Int, Pk_Int_1, Pk_Int_2, Pk."/=", True, "18");
   begin
      null;
   end;

   Result;
end Cc3601c;

-- C34006G.ADA

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
--     CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--     (IMPLICITLY) FOR DERIVED RECORD TYPES WITHOUT DISCRIMINANTS AND
--     WITH A LIMITED COMPONENT TYPE.

-- HISTORY:
--     JRK 08/24/87  CREATED ORIGINAL TEST.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with System; use System;
with Report; use Report;

procedure C34006g is

   package Pkg_L is

      type Lp is limited private;

      function Create (X : Integer) return Lp;

      function Equal (X, Y : Lp) return Boolean;

      procedure Assign (X : out Lp; Y : Lp);

      C1 : constant Lp;

   private

      type Lp is new Integer;

      C1 : constant Lp := 1;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      type Parent is record
         C : Component;
         B : Boolean := True;
      end record;

      function Equal (X, Y : Parent) return Boolean;

      function Aggr (C : Component; B : Boolean) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is new Parent;

   X : T;
   W : Parent;
   B : Boolean := False;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   package body Pkg_L is

      function Create (X : Integer) return Lp is
      begin
         return Lp (Ident_Int (X));
      end Create;

      function Equal (X, Y : Lp) return Boolean is
      begin
         return X = Y;
      end Equal;

      procedure Assign (X : out Lp; Y : Lp) is
      begin
         X := Y;
      end Assign;

   end Pkg_L;

   package body Pkg_P is

      function Equal (X, Y : Parent) return Boolean is
      begin
         return Equal (X.C, Y.C) and X.B = Y.B;
      end Equal;

      function Aggr (C : Component; B : Boolean) return Parent is
      begin
         return Result : Parent do
            Assign (Result.C, C);
            Result.B := B;
         end return;
      end Aggr;

   end Pkg_P;

begin
   Test
     ("C34006G",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "RECORD TYPES WITHOUT DISCRIMINANTS AND WITH A " &
      "LIMITED COMPONENT TYPE");

   Assign (X.C, Create (1));
   X.B := Ident_Bool (True);

   Assign (W.C, Create (1));
   W.B := Ident_Bool (True);

   if not Equal (T'(X), Aggr (C1, True)) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if not Equal (T (X), Aggr (C1, True)) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if not Equal (T (W), Aggr (C1, True)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if not Equal (Parent (X), Aggr (C1, True)) then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if not Equal (X.C, C1) or X.B /= True then
      Failed ("INCORRECT SELECTION (VALUE)");
   end if;

   X.B := Ident_Bool (False);
   if not Equal (X, Aggr (C1, False)) then
      Failed ("INCORRECT SELECTION (ASSIGNMENT)");
   end if;

   X.B := Ident_Bool (True);
   if not (X in T) then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT 'ADDRESS");
   end if;

   if X.C'First_Bit < 0 then
      Failed ("INCORRECT 'FIRST_BIT");
   end if;

   if X.C'Last_Bit < 0 or X.C'Last_Bit - X.C'First_Bit + 1 /= X.C'Size then
      Failed ("INCORRECT 'LAST_BIT");
   end if;

   if X.C'Position < 0 then
      Failed ("INCORRECT 'POSITION");
   end if;

   if X'Size < T'Size or X.C'Size < Component'Size or X.B'Size < Boolean'Size
   then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   Result;
end C34006g;

-- C34009A.ADA

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
--     (IMPLICITLY) FOR DERIVED NON-LIMITED PRIVATE TYPES WITHOUT
--     DISCRIMINANTS.

-- HISTORY:
--     JRK 08/28/87  CREATED ORIGINAL TEST.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34009a is

   package Pkg is

      type Parent is private;

      function Create (X : Integer) return Parent;

      function Con (X : Integer) return Parent;

   private

      type Parent is new Integer;

   end Pkg;

   use Pkg;

   type T is new Parent;

   X : T;
   K : Integer := X'Size;
   W : Parent;
   B : Boolean := False;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   package body Pkg is

      function Create (X : Integer) return Parent is
      begin
         return Parent (Ident_Int (X));
      end Create;

      function Con (X : Integer) return Parent is
      begin
         return Parent (X);
      end Con;

   end Pkg;

begin
   Test
     ("C34009A",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "NON-LIMITED PRIVATE TYPES WITHOUT " &
      "DISCRIMINANTS");

   X := Create (30);
   if X /= Con (30) then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= Con (30) then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= Con (30) then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   W := Create (-30);
   if T (W) /= Con (-30) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Parent (X) /= Con (30) then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if X = Con (0) then
      Failed ("INCORRECT =");
   end if;

   if X /= Con (30) then
      Failed ("INCORRECT /=");
   end if;

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

   Result;
end C34009a;

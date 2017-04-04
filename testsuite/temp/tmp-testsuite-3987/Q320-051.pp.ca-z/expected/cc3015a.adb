-- CC3015A.ADA

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
-- CHECK THAT WHEN A GENERIC PACKAGE INSTANTIATION IS ELABORATED,
-- STATEMENTS IN ITS PACKAGE BODY ARE EXECUTED AND EXPRESSIONS
-- REQUIRING EVALUATION ARE EVALUATED (E.G., DEFAULTS FOR OBJECT
-- DECLARATIONS ARE EVALUATED).

-- RJW 6/11/86

with Report; use Report;

procedure Cc3015a is
   Bool1, Bool2 : Boolean := False;

   type Enum is (Before, After);

   function F (I : Integer) return Integer is
   begin
      Bool2 := True;
      return I;
   end F;

   function Check (E : Enum) return Character is
   begin
      if E = Before then
         if Bool1 then
            Failed ("STATEMENT EXECUTED BEFORE " & "INSTANTIATION");
         end if;
         if Bool2 then
            Failed ("DEFAULT EXPRESSION EVALUATED " & "BEFORE INSTANTIATION");
         end if;
      else
         if Bool1 then
            null;
         else
            Failed ("STATEMENT NOT EXECUTED AT " & "INSTANTIATION");
         end if;
         if Bool2 then
            null;
         else
            Failed ("DEFAULT EXPRESSION NOT EVALUATED " & "AT INSTANTIATION");
         end if;
      end if;
      return 'A';
   end Check;

   generic
      type Int is range <>;
   package Pkg is
   end Pkg;

   package body Pkg is
      I : Int := Int'Val (F (0));
   begin
      Bool1 := True;
   end Pkg;

begin
   Test
     ("CC3015A",
      "CHECK THAT WHEN A GENERIC PACKAGE " &
      "INSTANTIATION IS ELABORATED, STATEMENTS " &
      "IN ITS PACKAGE BODY ARE EXECUTED AND " &
      "EXPRESSIONS REQUIRING EVALUATION ARE " &
      "EVALUATED (E.G., DEFAULTS FOR OBJECT " &
      "DECLARATIONS ARE EVALUATED)");

   declare
      A : Character := Check (Before);

      package Npkg is new Pkg (Integer);

      B : Character := Check (After);

   begin
      null;
   end;

   Result;
end Cc3015a;

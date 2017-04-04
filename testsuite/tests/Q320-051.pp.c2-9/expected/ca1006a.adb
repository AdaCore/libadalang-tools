-- CA1006A.ADA

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
-- CHECK THAT A LIBRARY UNIT AND ITS SUBUNITS CAN BE SUBMITTED TOGETHER FOR
-- COMPILATION.

-- JRK 5/14/81

with Report; use Report;

procedure Ca1006a is

   I : Integer := Ident_Int (0);

   package Call_Test is
   end Call_Test;

   package body Call_Test is
   begin
      Test
        ("CA1006A",
         "A LIBRARY UNIT AND ITS SUBUNITS " & "SUBMITTED TOGETHER");
   end Call_Test;

   function F (I : Integer) return Integer is separate;

   package Pkg is
      I : Integer := Ident_Int (0);
      procedure P (I : in out Integer);
   end Pkg;

   package body Pkg is separate;

   procedure P (I : in out Integer) is separate;

begin

   if Pkg.I /= 10 then
      Failed ("PACKAGE BODY STATEMENTS NOT EXECUTED");
   end if;

   if F (Ident_Int (5)) /= -5 then
      Failed ("FUNCTION NOT ELABORATED/EXECUTED");
   end if;

   Pkg.P (I);
   if I /= 3 then
      Failed ("PACKAGED PROCEDURE NOT ELABORATED/EXECUTED");
   end if;

   I := Ident_Int (-20);
   P (I);
   if I /= -24 then
      Failed ("PROCEDURE NOT ELABORATED/EXECUTED");
   end if;

   Result;
end Ca1006a;

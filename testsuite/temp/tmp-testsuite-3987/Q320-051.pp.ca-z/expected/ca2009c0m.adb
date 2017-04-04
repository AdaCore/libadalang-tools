-- CA2009C0M.ADA

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
--     CHECK THAT A GENERIC PACKAGE SUBUNIT CAN BE SPECIFIED AND
--     INSTANTIATED.  IN THIS TEST, THE SUBUNIT BODY IS IN A
--     SEPARATE FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST MUST RUN AND REPORT "PASSED" FOR ALL ADA 95 IMPLEMENTATIONS.

-- SEPARATE FILES ARE:
--     CA2009C0M  THE MAIN PROCEDURE.
--     CA2009C1   A SUBUNIT PACKAGE BODY (PKG1).

-- HISTORY:
--     BHS 08/01/84  CREATED ORIGINAL TEST.
--     BCB 01/05/88  MODIFIED HEADER.
--     EDS 08/04/98  REMOVE CONTROL Z AT END OF FILE.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.
--     RLB 09/15/99  REMOVED JUNK COMMENT.

with Report; use Report;
procedure Ca2009c0m is

   Int1 : Integer := 1;

   subtype Str15 is String (1 .. 15);
   Svar : Str15 := "ABCDEFGHIJKLMNO";

   generic
      type Item is private;
      Con1 : in Item;
      Var1 : in out Item;
   package Pkg1 is
   end Pkg1;

   package body Pkg1 is separate;

   package Ni_Pkg1 is new Pkg1 (Integer, Ident_Int (2), Int1);
   package Ns_Pkg1 is new Pkg1 (Str15, Ident_Str ("REINSTANTIATION"), Svar);

begin

   Test
     ("CA2009C",
      "SPECIFICATION AND INSTANTIATION " &
      "OF GENERIC PACKAGE SUBUNITS " &
      " -  SEPARATE FILES USED");

   if Int1 /= 2 then
      Failed ("INCORRECT INSTANTIATION - INTEGER");
   end if;

   if Svar /= "REINSTANTIATION" then
      Failed ("INCORRECT INSTANTIATION - STRING");
   end if;

   Result;

end Ca2009c0m;

-- C24207A.ADA

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
-- CHECK THAT LETTERS IN A BASED LITERAL MAY APPEAR IN UPPER OR LOWER CASE.

-- TBN 2/28/86

with Report; use Report;
procedure C24207a is

   type Float is digits 5;
   Int_1 : Integer := 15#AbC#;
   Int_2 : Integer := 15#aBc#;
   Flo_1 : Float   := 16#FeD.C#e1;
   Flo_2 : Float   := 16#fEd.c#E1;

begin
   Test
     ("C24207A",
      "CHECK THAT LETTERS IN A BASED LITERAL MAY " &
      "APPEAR IN UPPER OR LOWER CASE");

   if Int_1 /= Int_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 1");
   end if;

   if Flo_1 /= Flo_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 2");
   end if;

   Int_1 := 14#aBc#E1;
   Int_2 := 14#AbC#e1;
   Flo_1 := 16#CdEf.aB#E0;
   Flo_2 := 16#cDeF.Ab#e0;

   if Int_1 /= Int_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 3");
   end if;

   if Flo_1 /= Flo_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 4");
   end if;

   Result;
end C24207a;

-- CC3011D.ADA

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
-- CHECK THAT WHEN A GENERIC PACKAGE INSTANTIATION CONTAINS DECLARATIONS
-- OF SUBPROGRAMS WITH THE SAME SPECIFICATIONS, THE CALLS TO THE
-- SUBPROGRAMS ARE NOT AMBIGIOUS WITHIN THE GENERIC BODY.

-- SPS 5/7/82
-- SPS 2/7/83

with Report; use Report;

procedure Cc3011d is
begin
   Test
     ("CC3011D",
      "SUBPROGRAMS WITH SAME SPECIFICATIONS NOT" &
      " AMBIGIOUS WITHIN GENERIC BODY");

   declare
      type Flag is (Prt, Prs);
      Xx : Flag;

      generic
         type S is private;
         type T is private;
         V1 : S;
         V2 : T;
      package P1 is
         procedure Pr (X : S);
         procedure Pr (X : T);
      end P1;

      package body P1 is
         procedure Pr (X : S) is
         begin
            Xx := Prs;
         end Pr;

         procedure Pr (X : T) is
         begin
            Xx := Prt;
         end Pr;

      begin
         Xx := Prt;
         Pr (V1);
         if Xx /= Prs then
            Failed ("WRONG BINDING FOR PR WITH TYPE S");
         end if;
         Xx := Prs;
         Pr (V2);
         if Xx /= Prt then
            Failed ("WRONG BINDING FOR PR WITH TYPE T");
         end if;
      end P1;

      package Pak is new P1 (Integer, Integer, 1, 2);

   begin
      null;
   end;

   Result;
end Cc3011d;

-- C87B24A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:

-- THE PREFIX OF A SLICE MUST BE APPROPRIATE FOR A ONE DIMENSIONAL
-- ARRAY TYPE.

-- TRH  26 JULY 82

with Report; use Report;

procedure C87b24a is

   type List is array (1 .. 5) of Integer;
   type Grid is array (1 .. 5, 1 .. 5) of Integer;
   type Cube is array (1 .. 5, 1 .. 5, 1 .. 5) of Integer;
   type Hype is array (1 .. 5, 1 .. 5, 1 .. 5, 1 .. 5) of Integer;
   type Flag is (Pass, Fail);

   L : List := (1 .. 5 => 0);
   G : Grid := (1 .. 5 => (1 .. 5 => 0));
   C : Cube := (1 .. 5 => (1 .. 5 => (1 .. 5 => 0)));
   H : Hype := (1 .. 5 => (1 .. 5 => (1 .. 5 => (1 .. 5 => 0))));

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("SLICE PREFIX MUST BE APPROPRIATE FOR ONE " & "DIMENSIONAL ARRAY");
      end if;
      return Arg;
   end F1;

   function F2 is new F1 (List, L, Pass);
   function F2 is new F1 (Grid, G, Fail);
   function F2 is new F1 (Cube, C, Fail);
   function F2 is new F1 (Hype, H, Fail);

begin
   Test
     ("C87B24A",
      "OVERLOADED PREFIX FOR SLICE RESOLVED TO " &
      "ONE DIMENSIONAL ARRAY TYPE");

   declare
      S1 : Integer;

   begin
      S1 := F2 (2 .. 3) (2);
   end;

   Result;
end C87b24a;

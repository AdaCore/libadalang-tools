-- C41201D.ADA

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
-- FOR SLICED COMPONENTS OF THE FORM F(...), CHECK THAT
--   THE REQUIREMENT FOR A ONE-DIMENSIONAL ARRAY AND THE
--   TYPE OF THE INDEX ARE USED TO RESOLVE AN OVERLOADING OF F.

-- WKB 8/11/81
-- JBG 10/12/81
-- SPS 11/1/82

with Report;
procedure C41201d is

   use Report;

   type T is array (Integer range <>) of Integer;
   subtype T1 is T (1 .. 10);
   type T2 is array (1 .. 10, 1 .. 10) of Integer;
   Tt : T (1 .. 3);

   subtype U1 is T (1 .. 10);
   type U2 is (Mon, Tue, Wed, Thu, Fri);
   subtype Su2 is U2 range Mon .. Thu;
   type U3 is array (Su2) of Integer;
   Uu : T (1 .. 3);

   type V is array (Integer range <>) of Boolean;
   subtype V1 is V (1 .. 10);
   subtype V2 is T (1 .. 10);
   Vv : V (2 .. 5);

   function F return T1 is
   begin
      return (1, 1, 1, 1, 5, 6, 7, 8, 9, 10);
   end F;

   function F return T2 is
   begin
      return (1 .. 10 => (1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
   end F;

   function G return U1 is
   begin
      return (3, 3, 3, 3, 5, 6, 7, 8, 9, 10);
   end G;

   function G return U3 is
   begin
      return (0, 1, 2, 3);
   end G;

   function H return V1 is
   begin
      return (1 | 3 .. 10 => False, 2 => Ident_Bool (True));
   end H;

   function H return V2 is
   begin
      return (1 .. 10 => 5);
   end H;

begin

   Test
     ("C41201D",
      "WHEN SLICING FUNCTION RESULTS, TYPE OF " &
      "RESULT IS USED FOR OVERLOADING RESOLUTION");

   if F (1 .. 3) /=
     F (Ident_Int (2) .. Ident_Int (4))
   then -- NUMBER OF DIMENSIONS.
      Failed ("WRONG VALUE - 1");
   end if;

   if G (1 .. 3) /= G (Ident_Int (2) .. Ident_Int (4)) then -- INDEX TYPE.
      Failed ("WRONG VALUE - 2");
   end if;

   if not Ident_Bool (H (2 .. 3) (2)) then    -- COMPONENT TYPE.
      Failed ("WRONG VALUE - 3");
   end if;

   Result;

end C41201d;

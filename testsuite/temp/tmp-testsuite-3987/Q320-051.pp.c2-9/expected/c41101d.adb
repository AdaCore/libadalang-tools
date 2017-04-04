-- C41101D.ADA

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
-- FOR INDEXED COMPONENTS OF THE FORM F(...), CHECK THAT
--   THE NUMBER OF INDEX VALUES, THE TYPE OF THE INDEX
--   VALUES, AND THE REQUIRED TYPE OF THE INDEXED COMPONENT
--   ARE USED TO RESOLVE AN OVERLOADING OF F.

-- WKB 8/12/81
-- JBG 10/12/81
-- SPS 11/1/82

with Report;
procedure C41101d is

   use Report;

   type T1 is array (1 .. 10) of Integer;
   type T2 is array (1 .. 10, 1 .. 10) of Integer;
   I : Integer;

   type U1 is (Mon, Tue, Wed, Thu, Fri);
   type U2 is array (U1 range Mon .. Thu) of Integer;

   type V1 is array (1 .. 10) of Boolean;
   B : Boolean;

   function F return T1 is
   begin
      return (1 .. 10 => 1);
   end F;

   function F return T2 is
   begin
      return (1 .. 10 => (1 .. 10 => 2));
   end F;

   function G return U2 is
   begin
      return (Mon .. Thu => 3);
   end G;

   function G return T1 is
   begin
      return (1 .. 10 => 4);
   end G;

   function H return T1 is
   begin
      return (1 .. 10 => 5);
   end H;

   function H return V1 is
   begin
      return (1 .. 10 => False);
   end H;

begin

   Test
     ("C41101D",
      "WHEN INDEXING FUNCTION RESULTS, INDEX TYPE, " &
      "NUMBER OF INDICES, AND COMPONENT TYPE ARE " &
      "USED FOR OVERLOADING RESOLUTION");

   I := F (7);              -- NUMBER OF INDEX VALUES.
   if I /= Ident_Int (1) then
      Failed ("WRONG VALUE - 1");
   end if;

   I := G (3);              -- INDEX TYPE.
   if I /= Ident_Int (4) then
      Failed ("WRONG VALUE - 2");
   end if;

   B := H (5);              -- COMPONENT TYPE.
   if B /= Ident_Bool (False) then
      Failed ("WRONG VALUE - 3");
   end if;

   Result;

end C41101d;

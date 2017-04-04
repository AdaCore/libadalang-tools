-- C36180A.ADA

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
--     CHECK THAT AN INDEX CONSTRAINT CAN HAVE THE FORM A'RANGE,
--     WHERE A IS A PREVIOUSLY DECLARED ARRAY OBJECT OR CONSTRAINED
--     ARRAY SUBTYPE.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C36180a is

   type J is array (Integer range <>) of Integer;

   type K is array (1 .. 10) of Integer;

   subtype A is J (0 .. 50);

   subtype W is J (A'Range);

   subtype X is J (K'Range);

   type Y is access J;

   type Z is access J;

   type F is new J (A'Range);

   type G is new J (K'Range);

   B : array (A'Range) of Integer;

   C : array (K'Range) of Integer;

   D : array (1 .. 10) of Integer;

   E : array (D'Range) of Integer;

   H : J (A'Range);

   I : J (K'Range);

   L : J (D'Range);

   V1 : W;

   V2 : X;

   V3 : Y := new J (A'Range);

   V4 : Z := new J (K'Range);

   V5 : F;

   V6 : G;

begin
   Test
     ("C36180A",
      "CHECK THAT AN INDEX CONSTRAINT CAN HAVE THE " &
      "FORM A'RANGE, WHERE A IS A PREVIOUSLY " &
      "DECLARED ARRAY OBJECT OR CONSTRAINED ARRAY " &
      "SUBTYPE");

   if B'First /= Ident_Int (0) or B'Last /= Ident_Int (50) then
      Failed ("IMPROPER VALUE FOR B'FIRST OR B'LAST");
   end if;

   if C'First /= Ident_Int (1) or C'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR C'FIRST OR C'LAST");
   end if;

   if E'First /= Ident_Int (1) or E'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR E'FIRST OR E'LAST");
   end if;

   if H'First /= Ident_Int (0) or H'Last /= Ident_Int (50) then
      Failed ("IMPROPER VALUE FOR H'FIRST OR H'LAST");
   end if;

   if I'First /= Ident_Int (1) or I'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR I'FIRST OR I'LAST");
   end if;

   if L'First /= Ident_Int (1) or L'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR L'FIRST OR L'LAST");
   end if;

   if V1'First /= Ident_Int (0) or V1'Last /= Ident_Int (50) then
      Failed ("IMPROPER VALUE FOR V1'FIRST OR V1'LAST");
   end if;

   if V2'First /= Ident_Int (1) or V2'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR V2'FIRST OR V2'LAST");
   end if;

   if V3.all'First /= Ident_Int (0) or V3.all'Last /= Ident_Int (50) then
      Failed ("IMPROPER VALUE FOR V3'FIRST OR V3'LAST");
   end if;

   if V4.all'First /= Ident_Int (1) or V4.all'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR V4'FIRST OR V4'LAST");
   end if;

   if V5'First /= Ident_Int (0) or V5'Last /= Ident_Int (50) then
      Failed ("IMPROPER VALUE FOR V5'FIRST OR V5'LAST");
   end if;

   if V6'First /= Ident_Int (1) or V6'Last /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR V6'FIRST OR V6'LAST");
   end if;

   Result;
end C36180a;

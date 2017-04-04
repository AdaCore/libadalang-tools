-- C55B06A.ADA

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
-- CHECK THAT LOOPS MAY BE SPECIFIED FOR BOOLEAN, INTEGER, CHARACTER,
-- ENUMERATION, AND DERIVED TYPES, INCLUDING TYPES DERIVED FROM DERIVED
-- TYPES. DERIVED BOOLEAN IS NOT TESTED IN THIS TEST.

-- DAT 3/26/81
-- JBG 9/29/82
-- SPS 3/11/83
-- JBG 10/5/83
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C55b06a is

   type Enum is ('A', 'B', 'D', 'C', Z, X, D, A, C);

   type D1 is new Character range 'A' .. 'Z';
   type D2 is new Integer;
   type D3 is new Enum;
   type D4 is new D1;
   type D5 is new D2;
   type D6 is new D3;

   One      : Integer := Ident_Int (1);
   Count    : Integer := 0;
   Oldcount : Integer := 0;

   procedure Q is
   begin
      Count := Count + One;
   end Q;

begin
   Test ("C55B06A", "TEST LOOPS FOR ALL DISCRETE TYPES");

   for I in Boolean loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 1");
   end if;
   Oldcount := Count;

   for I in False .. True loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 2");
   end if;
   Oldcount := Count;

   for I in Boolean range False .. True loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 3");
   end if;
   Oldcount := Count;

   for I in Integer loop
      Q;
      exit when I = Integer'First + 2;
   end loop;
   if Oldcount + Ident_Int (3) /= Count then
      Failed ("LOOP 4");
   end if;
   Oldcount := Count;

   for I in 3 .. Ident_Int (5) loop
      Q;
   end loop;
   if Oldcount + Ident_Int (3) /= Count then
      Failed ("LOOP 5");
   end if;
   Oldcount := Count;

   for I in Integer range -2 .. -1 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 6");
   end if;
   Oldcount := Count;

   for I in Integer range Integer'First .. Integer'First + 1 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 7");
   end if;
   Oldcount := Count;

   for I in 'A' .. Character'('Z') loop
      Q;
   end loop;
   if Oldcount + Ident_Int (26) /= Count then
      Failed ("LOOP 9");
   end if;
   Oldcount := Count;

   for I in Character range 'A' .. 'D' loop
      Q;
   end loop;
   if Oldcount + Ident_Int (4) /= Count then
      Failed ("LOOP 10");
   end if;
   Oldcount := Count;

   for I in Enum loop
      Q;
   end loop;
   if Oldcount + Ident_Int (9) /= Count then
      Failed ("LOOP 11");
   end if;
   Oldcount := Count;

   for I in Enum range D .. C loop
      Q;
   end loop;
   if Oldcount + Ident_Int (3) /= Count then
      Failed ("LOOP 12");
   end if;
   Oldcount := Count;

   for I in 'A' .. Enum'(Z) loop
      Q;
   end loop;
   if Oldcount + Ident_Int (5) /= Count then
      Failed ("LOOP 13");
   end if;
   Oldcount := Count;

   for I in D1 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (26) /= Count then
      Failed ("LOOP 14");
   end if;
   Oldcount := Count;

   for I in D1 range 'A' .. 'Z' loop
      Q;
   end loop;
   if Oldcount + Ident_Int (26) /= Count then
      Failed ("LOOP 15");
   end if;
   Oldcount := Count;

   for I in D1'('A') .. 'D' loop
      Q;
   end loop;
   if Oldcount + Ident_Int (4) /= Count then
      Failed ("LOOP 16");
   end if;
   Oldcount := Count;

   for I in D2 loop
      Q;
      if I > D2'First + 3 then
         exit;
      end if;
   end loop;
   if Oldcount + Ident_Int (5) /= Count then
      Failed ("LOOP 17");
   end if;
   Oldcount := Count;

   for I in D2 range -100 .. -99 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 18");
   end if;
   Oldcount := Count;

   for I in D2'(1) .. 2 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 19");
   end if;
   Oldcount := Count;

   for I in D3 loop
      if I in 'A' .. 'C' then
         Q;        -- 4
      else
         Q;
         Q;     -- 10
      end if;
   end loop;
   if Oldcount + Ident_Int (14) /= Count then
      Failed ("LOOP 20");
   end if;
   Oldcount := Count;

   for I in D3 range 'A' .. Z loop
      Q;
   end loop;
   if Oldcount + Ident_Int (5) /= Count then
      Failed ("LOOP 21");
   end if;
   Oldcount := Count;

   for I in 'A' .. D3'(Z) loop
      Q;
   end loop;
   if Oldcount + Ident_Int (5) /= Count then
      Failed ("LOOP 22");
   end if;
   Oldcount := Count;

   for I in D4 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (26) /= Count then
      Failed ("LOOP 23");
   end if;
   Oldcount := Count;

   for I in D4'('A') .. 'Z' loop
      Q;
   end loop;
   if Oldcount + Ident_Int (26) /= Count then
      Failed ("LOOP 24");
   end if;
   Oldcount := Count;

   for I in D4 range 'B' .. 'D' loop
      Q;
   end loop;
   if Oldcount + Ident_Int (3) /= Count then
      Failed ("LOOP 25");
   end if;
   Oldcount := Count;

   for J in D5 loop
      Q;        -- 4
      exit when J = D5 (Integer'First) + 3;
      Q;        -- 3
   end loop;
   if Oldcount + Ident_Int (7) /= Count then
      Failed ("LOOP 26");
   end if;
   Oldcount := Count;

   for J in D5 range -2 .. -1 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (2) /= Count then
      Failed ("LOOP 27");
   end if;
   Oldcount := Count;

   for J in D5'(-10) .. D5'(-6) loop
      Q;
   end loop;
   if Oldcount + Ident_Int (5) /= Count then
      Failed ("LOOP 28");
   end if;
   Oldcount := Count;

   for J in D6 loop
      Q;
   end loop;
   if Oldcount + Ident_Int (9) /= Count then
      Failed ("LOOP 29");
   end if;
   Oldcount := Count;

   for J in D6 range Z .. A loop
      Q;
   end loop;
   if Oldcount + Ident_Int (4) /= Count then
      Failed ("LOOP 30");
   end if;
   Oldcount := Count;

   for J in D6'('D') .. D loop
      Q;
   end loop;
   if Oldcount + Ident_Int (5) /= Count then
      Failed ("LOOP 31");
   end if;
   Oldcount := Count;

   Result;
end C55b06a;

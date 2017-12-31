-- C35503B.ADA

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
-- CHECK THAT 'WIDTH' YIELDS THE CORRECT RESULT WHEN THE PREFIX IS A GENERIC
-- FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS AN INTEGER TYPE.

-- RJW 3/17/86

with Report; use Report;

procedure C35503b is

begin
   Test
     ("C35503B",
      "CHECK THAT 'WIDTH' YIELDS THE CORRECT " &
      "RESULT WHEN THE PREFIX IS A GENERIC FORMAL " &
      "DISCRETE TYPE WHOSE ACTUAL PARAMETER IS AN " & "INTEGER TYPE");

   declare

      type Int is range -1_000 .. 1_000;
      type Int2 is new Int range 0E8 .. 1E3;
      subtype Sint1 is Int range 00_000 .. 300;
      subtype Sint2 is Int range 16#E#E1 .. 2#1111_1111#;

      generic
         type I is (<>);
         W : Integer;
      procedure P (Str : String);

      procedure P (Str : String) is
         subtype Subi is I range I'Val (Ident_Int (224)) .. I'Val (255);
         subtype Norange is I range I'Val (255) .. I'Val (Ident_Int (224));
      begin
         if Ident_Int (I'Width) /= W then
            Failed ("INCORRECT I'WIDTH FOR " & Str);
         end if;

         if Ident_Int (Subi'Width) /= 4 then
            Failed ("INCORRECT SUBI'WIDTH FOR " & Str);
         end if;

         if Ident_Int (Norange'Width) /= 0 then
            Failed ("INCORRECT NORANGE'WIDTH FOR " & Str);
         end if;
      end P;

      procedure P_Integer is new P (Integer, Integer'Width);
      procedure P_Int is new P (Int, 5);
      procedure P_Int2 is new P (Int2, 5);
      procedure P_Sint1 is new P (Sint1, 4);
      procedure P_Sint2 is new P (Sint2, 4);

   begin
      P_Integer ("'INTEGER'");
      P_Int ("'INT'");
      P_Int2 ("'INT2'");
      P_Sint1 ("'SINT1'");
      P_Sint2 ("'SINT2'");
   end;

   Result;
end C35503b;

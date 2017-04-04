-- CE3908A.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES CAN OPERATE ON STRINGS.
--     CHECK THAT IT RAISES END_ERROR WHEN THE STRING IS NULL OR
--     EMPTY.  CHECK THAT LAST CONTAINS THE INDEX VALUE OF THE LAST
--     CHARACTER READ FROM THE STRING.

-- HISTORY:
--     SPS 10/11/82
--     VKG 01/06/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     DWC 09/18/87  ADDED CASES WHICH CONTAIN TABS WITH AND WITHOUT
--                   ENUMERATION LITERALS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3908a is
begin

   Test
     ("CE3908A",
      "CHECK THAT GET FOR ENUMERATION TYPES CAN " &
      "OPERATE ON STRINGS.  CHECK THAT IT RAISES " &
      "END_ERROR WHEN THE STRING IS NULL OR EMPTY.  " &
      "CHECK THAT LAST CONTAINS THE INDEX VALUE OF " &
      "THE LAST CHARACTER READ FROM THE STRING");

   declare
      type Fruit is (Apple, Pear, Orange, Strawberry);
      Dessert : Fruit;
      package Fruit_Io is new Enumeration_Io (Fruit);
      use Fruit_Io;
      L : Positive;
   begin
      Get ("APPLE  ", Dessert, L);
      if Dessert /= Apple then
         Failed ("ENUMERATION VALUE FROM STRING INCORRECT - 1");
      end if;

      if L /= Ident_Int (5) then
         Failed ("LAST CONTAINS INCORRECT VALUE AFTER GET - 1");
      end if;

      Get ("APPLE", Dessert, L);
      if Dessert /= Apple then
         Failed ("ENUMERATION VALUE FROM STRING INCORRECT - 2");
      end if;

      if L /= Ident_Int (5) then
         Failed ("LAST CONTAINS INCORRECT VALUE AFTER GET - 2");
      end if;

      begin
         Get (Ascii.Ht & "APPLE", Dessert, L);
         if Dessert /= Apple then
            Failed ("ENUMERATION VALUE FROM STRING " & "INCORRECT - 3");
         end if;
         if L /= Ident_Int (6) then
            Failed ("LAST CONTAINS INCORRECT VALUE AFTER " & "GET - 3");
         end if;
      exception
         when End_Error =>
            Failed ("GET DID NOT SKIP LEADING TABS");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 3");
      end;

-- NULL STRING LITERAL.

      begin
         Get ("", Dessert, L);
         Failed ("END_ERROR NOT RAISED - 4");
      exception
         when End_Error =>
            if L /= Ident_Int (6) then
               Failed ("LAST CONTAINS INCORRECT VALUE " & "AFTER GET - 4");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 4");
      end;

      begin
         Get (Ascii.Ht & "", Dessert, L);
         Failed ("END_ERROR NOT RAISED - 5");
      exception
         when End_Error =>
            if L /= Ident_Int (6) then
               Failed ("LAST CONTAINS INCORRECT VALUE " & "AFTER GET - 5");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 5");
      end;

-- STRING LITERAL WITH BLANKS.

      begin
         Get ("     ", Dessert, L);
         Failed ("END ERROR NOT RAISED - 6");
      exception
         when End_Error =>
            if L /= Ident_Int (6) then
               Failed ("LAST CONTAINS INCORRECT VALUE " & "AFTER GET - 6");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 6");
      end;

   end;

   Result;
end Ce3908a;

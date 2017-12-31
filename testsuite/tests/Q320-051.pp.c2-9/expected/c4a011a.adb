-- C4A011A.ADA

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
-- CHECK THAT NONSTATIC UNIVERSAL REAL EXPRESSIONS ARE EVALUATED WITH THE
-- ACCURACY OF THE MOST PRECISE PREDEFINED FLOATING POINT TYPE (I. E., THE
-- TYPE FOR WHICH 'DIGITS EQUALS SYSTEM.MAX_DIGITS).

-- RJW 8/4/86

with System; use System;
with Report; use Report;

procedure C4a011a is

   type Max_Float is digits Max_Digits;

   C5l : constant := 16#0.AAAA_8#;
   C5u : constant := 16#0.AAAA_C#;

   C6l : constant := 16#0.AAAA_A8#;
   C6u : constant := 16#0.AAAA_B0#;

   C7l : constant := 16#0.AAAA_AA8#;
   C7u : constant := 16#0.AAAA_AB0#;

   C8l : constant := 16#0.AAAA_AAA#;
   C8u : constant := 16#0.AAAA_AAB#;

   C9l : constant := 16#0.AAAA_AAAA#;
   C9u : constant := 16#0.AAAA_AAAC#;

   C10l : constant := 16#0.AAAA_AAAA_A#;
   C10u : constant := 16#0.AAAA_AAAA_C#;

   C11l : constant := 16#0.AAAA_AAAA_A8#;
   C11u : constant := 16#0.AAAA_AAAA_AC#;

   C12l : constant := 16#0.AAAA_AAAA_AA8#;
   C12u : constant := 16#0.AAAA_AAAA_AB0#;

   C13l : constant := 16#0.AAAA_AAAA_AAA8#;
   C13u : constant := 16#0.AAAA_AAAA_AAB0#;

   C14l : constant := 16#0.AAAA_AAAA_AAAA#;
   C14u : constant := 16#0.AAAA_AAAA_AAAB#;

   C15l : constant := 16#0.AAAA_AAAA_AAAA_A#;
   C15u : constant := 16#0.AAAA_AAAA_AAAA_C#;

   C16l : constant := 16#0.AAAA_AAAA_AAAA_AA#;
   C16u : constant := 16#0.AAAA_AAAA_AAAA_AC#;

   C17l : constant := 16#0.AAAA_AAAA_AAAA_AA8#;
   C17u : constant := 16#0.AAAA_AAAA_AAAA_AAC#;

   C18l : constant := 16#0.AAAA_AAAA_AAAA_AAA8#;
   C18u : constant := 16#0.AAAA_AAAA_AAAA_AAB0#;

   C19l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_8#;
   C19u : constant := 16#0.AAAA_AAAA_AAAA_AAAB_0#;

   C20l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_A#;
   C20u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_B#;

   C21l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AA#;
   C21u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AC#;

   C22l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAA#;
   C22u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAC#;

   C23l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAA8#;
   C23u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAC#;

   C24l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_8#;
   C24u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAB_0#;

   C25l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_A8#;
   C25u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_B0#;

   C26l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AA#;
   C26u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AB#;

   C27l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAA#;
   C27u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAC#;

   C28l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA#;
   C28u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAC#;

   C29l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_8#;
   C29u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_C#;

   C30l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_A8#;
   C30u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_B0#;

   C31l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AA#;
   C31u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AB#;

   C32l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAA#;
   C32u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAB#;

   C33l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA#;
   C33u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAC#;

   C34l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_8#;
   C34u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_C#;

   C35l : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_A8#;
   C35u : constant := 16#0.AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AC#;

begin

   Test
     ("C4A011A",
      "CHECK THAT NONSTATIC UNIVERSAL REAL " &
      "EXPRESSIONS ARE EVALUATED WITH THE " &
      "ACCURACY OF THE MOST PRECISE PREDEFINED " &
      "FLOATING POINT TYPE (I. E., THE TYPE FOR " &
      "WHICH 'DIGITS EQUALS SYSTEM.MAX_DIGITS");

   case Max_Digits is
      when 5 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C5l .. C5u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 5");
         end if;
      when 6 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C6l .. C6u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 6");
         end if;
      when 7 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C7l .. C7u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 7");
         end if;
      when 8 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C8l .. C8u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 8");
         end if;
      when 9 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C9l .. C9u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 9");
         end if;
      when 10 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C10l .. C10u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 10");
         end if;
      when 11 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C11l .. C11u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 11");
         end if;
      when 12 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C12l .. C12u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 12");
         end if;
      when 13 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C13l .. C13u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 13");
         end if;
      when 14 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C14l .. C14u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 14");
         end if;
      when 15 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C15l .. C15u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 15");
         end if;
      when 16 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C16l .. C16u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 16");
         end if;
      when 17 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C17l .. C17u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 17");
         end if;
      when 18 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C18l .. C18u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 18");
         end if;
      when 19 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C19l .. C19u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 19");
         end if;
      when 20 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C20l .. C20u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 20");
         end if;
      when 21 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C21l .. C21u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 21");
         end if;
      when 22 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C22l .. C22u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 22");
         end if;
      when 23 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C23l .. C23u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 23");
         end if;
      when 24 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C24l .. C24u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 24");
         end if;
      when 25 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C25l .. C25u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 25");
         end if;
      when 26 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C26l .. C26u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 26");
         end if;
      when 27 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C27l .. C27u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 27");
         end if;
      when 28 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C28l .. C28u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 28");
         end if;
      when 29 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C29l .. C29u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 29");
         end if;
      when 30 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C30l .. C30u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 30");
         end if;
      when 31 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C31l .. C31u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 31");
         end if;
      when 32 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C32l .. C32u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 32");
         end if;
      when 33 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C33l .. C33u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 33");
         end if;
      when 34 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C34l .. C34u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 34");
         end if;
      when 35 =>
         if (2.0 * Integer'Pos (Ident_Int (1))) / 3.0 not in C35l .. C35u then
            Failed ("INCORRECT ACCURACY FOR A MAX_DIGITS " & "VALUE OF 35");
         end if;
      when others =>
         Not_Applicable
           ("MAX_DIGITS OUT OF RANGE OF TEST.  " & "MAX_DIGITS = " &
            Integer'Image (Max_Digits));
   end case;

   Result;

end C4a011a;
